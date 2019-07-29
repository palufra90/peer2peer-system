-module(dht).
-import(lists, [foreach/2, nth/2, append/2, min/1, max/1, filter/2, subtract/2, delete/2]).
-import(aux, [get_timestamp/0, match/3, binary/1]).
-export([search_oldest_job/1, 
         get_status/2, 
         get_job/2, 
         get_job_result/2, 
         get_job_node/2,
         get_beam/2, 
         get_candidates/2, 
         get_my_jobs/1, 
         list_my_jobs/1, 
         add_important_job/4, 
         add_job/4,
         add_candidate/3, 
         return_ready/3,
         empty_candidates/2, 
         running_job/2, 
         nodedown_job/2, 
         terminated_job/3,
         delete_my_job/2,
         delete_my_jobs/1, 
         check_my_jobs/1, 
         delete_my_terminated_jobs/1,
         reset_or_delete/3,
         reset_all/1]).
-include("dht.hrl").

%==========
% SEARCH
%==========

%searches job according to the state
% @private
search_jobs(Pid, State) ->
   	riakc_pb_socket:get_index_eq(Pid,
                                 ?JOB_BUCKET, 
                                 {binary_index, "status"}, 
                                 State).

%searches the oldest important job, then the oldest ready job
search_oldest_job(Pid) ->

   Important = search_jobs(Pid, ?IMPORTANT),
   ListImportant = element(2, element(2, Important)),
   
   if (ListImportant =/= []) -> Oldest = min(ListImportant),
				                Result = [Oldest, ?IMPORTANT];
	true ->	Ready = search_jobs(Pid, ?READY),
       		ListReady = element(2, element(2, Ready)),
		if (ListReady =/= []) -> Oldest = min(ListReady),
					             Result = [Oldest, ?READY];
		true ->	Result = [notfound, notfound]
		end
   end,

   Result.

%=========
% READING 
%=========

%returns the status of the job
get_status(Pid, KeyJob) ->
	BKeyJob = binary(KeyJob),
    {Res, Job} = riakc_pb_socket:get(Pid, 
                                     ?JOB_BUCKET, 
                                     BKeyJob),
	if 	Res == ok ->	
    		MD1 = riakc_obj:get_update_metadata(Job),
    		Status = nth(1,riakc_obj:get_secondary_index(MD1,{binary_index, "status"})),
    		Status;
		true ->
			{Res, Job}
	end.

%reads the job
get_job(Pid, KeyJob) -> 
	BKeyJob = binary(KeyJob),
    {Res, RJob} = riakc_pb_socket:get(Pid, 
                                     ?JOB_BUCKET, 
                                     BKeyJob),
	if 	Res == ok ->    
            binary_to_term(riakc_obj:get_value(RJob));
        true ->
            {Res, RJob}
    end.

%reads the job result
get_job_result(Pid, KeyJob) ->
    Res = get_job(Pid, KeyJob),
    if 	element(1,Res) == error -> 
            Res;
        true ->
            element(7, Res)
    end.
    
%reads the node that insert the job
get_job_node(Pid, KeyJob) ->
    Res = get_job(Pid, KeyJob),
    if 	element(1,Res) == error -> 
            Res;
        true ->
            element(3, Res)
    end.

%reads the binary
get_beam(Pid, ModuleName) -> 
    {Res, Module} = riakc_pb_socket:get(Pid, 
                                        ?MODULE_BUCKET, 
                                        atom_to_binary(ModuleName,latin1)),
    if 	Res == ok -> 
            binary_to_term(riakc_obj:get_value(Module));
        true ->
            {Res, Module} 
    end.

%reads the candidates list
get_candidates(Pid, KeyJob) -> 
	BKeyJob = binary(KeyJob),
    {Res, Nodes} = riakc_pb_socket:get(Pid, 
                                        ?CANDIDATE_BUCKET, 
                                        BKeyJob),
    if 	Res == ok ->
    		binary_to_term(riakc_obj:get_value(Nodes));
    	true ->
    		{Res, Nodes}
    end.


%reads the object with node jobs list
get_my_jobs(Pid)->
    {Res, Job} = riakc_pb_socket:get(Pid, 
                                    ?MY_JOB_BUCKET, 
                                    atom_to_binary(node(), latin1)),
    if 	Res == ok ->
    		binary_to_term(riakc_obj:get_value(Job));
    	true -> 
            empty_my_jobs(Pid), 
            {ok, MyEmptyJobs} = riakc_pb_socket:get(Pid, 
                                    ?MY_JOB_BUCKET, 
                                    atom_to_binary(node(), latin1)),
            binary_to_term(riakc_obj:get_value(MyEmptyJobs))
    end.

%returns node jobs list
list_my_jobs(Pid) ->
    element(3, get_my_jobs(Pid)).
    
%=========
% WRITING
%=========

%adds an important job, with high priority
add_important_job(Pid, Module, Function, Args)->
    add_job(Pid, Module, Function, Args, ?IMPORTANT).

%adds a job
add_job(Pid, Module, Function, Args)->
    add_job(Pid, Module, Function, Args, ?READY).

%inserts job and loads the binary
%checks if the module already exists
% @private
add_job(Pid, Module, Function, Args, Priority)->
    KeyJob = get_timestamp(),
    BinaryKeyJob = list_to_binary(KeyJob),
    NewJob = #job{time = KeyJob,
    	          node = node(),
		          module = Module,
		          function = Function,
		          args = Args,
		          result = noresult},
	Res = add_module(Pid, Module),
   	case Res of
   			ok ->
    			save_job(Pid, NewJob, Priority),
    			empty_candidates(Pid, BinaryKeyJob),
    			add_my_job(Pid, BinaryKeyJob),
    			io:format("Job inserted in bucket ~p with key: ~p ~n", [?JOB_BUCKET, BinaryKeyJob]);
    		error ->
    			io:format("Job not inserted, there is already a different module of the same name, modify module name: ~p ~n",[Module]),
    			error;
    		error1 ->
    			io:format("Job not inserted, module ~p not in path ~n", [Module]),
    		    error
    end.

%inserts binary into bucket Code
% @private
add_module(Pid, Module) ->
    Code = code:get_object_code(Module),
    if 	Code == error -> 
    		error1;
	   	true -> verify_module(Pid, Module, Code)
    end.

%verifies if that module has been already stored in the bucket 
%and if a different module with the same name already exists
% @private
verify_module(Pid, Module, Code) ->
    {M, B, _} = Code,
    {Res, _} = riakc_pb_socket:get(Pid, ?MODULE_BUCKET, atom_to_binary(Module, latin1)),
    if  Res == ok ->
			Match = match(Pid, Module, B),
			if 	Match == false ->
					error;
				true -> io:format("Module ~p already exists ~n",[Module])
			end;
		true ->   
    		NewModule = #code{module = M,
	  			              binary = B},
    		save_module(Pid, NewModule)	
    end.	
		
%adds a candidate to the candidates list	
% @private
add_candidate(Pid, KeyJob, Performance)->
    {_, KeyObj, Candidates} = get_candidates(Pid, KeyJob),
    UpdateWorkers = #candidates{job = KeyObj,
	  			                nodes = append(Candidates, [[node(), Performance]])},
    save_candidate(Pid, UpdateWorkers).

%adds job to the node job list
% @private
add_my_job(Pid, KeyJob)->
    Jobs = list_my_jobs(Pid),
    UpdateMyJobs = #myJobs{node = node(),
	  		               jobs = append(Jobs, [KeyJob])},
    save_my_jobs(Pid, UpdateMyJobs).

%saves job in dht, key=timestamp, secondary index=status
% @private
save_job(Pid, Job, Priority) -> 
    KeyJob = list_to_binary(Job#job.time),
    NewJob = riakc_obj:new(?JOB_BUCKET, 
                           KeyJob,  
                           Job),
    riakc_pb_socket:put(Pid, NewJob),
    set_secondary_index(Pid, KeyJob, Priority).

%saves binary in dht, key=module name
% @private
save_module(Pid, Module) -> 
    RModule = riakc_obj:new(?MODULE_BUCKET, 
                            atom_to_binary(Module#code.module, latin1), 
                            Module),
    riakc_pb_socket:put(Pid, RModule).

%saves candidates list, key=job timestamp
% @private
save_candidate(Pid, List) ->
    RCandidates = riakc_obj:new(?CANDIDATE_BUCKET,
				List#candidates.job,
				List),
    riakc_pb_socket:put(Pid, RCandidates).

%saves a job in the node jobs list
% @private
save_my_jobs(Pid, JobList) ->
    MyJobs = riakc_obj:new(?MY_JOB_BUCKET,
			   atom_to_binary(JobList#myJobs.node, latin1),
			   JobList),
    riakc_pb_socket:put(Pid, MyJobs).

%sets the secondary index
% @private
set_secondary_index(Pid, KeyJob, Status) ->
     {ok, Job} = riakc_pb_socket:get(Pid, 
                                     ?JOB_BUCKET, 
                                     KeyJob),
     MD1 = riakc_obj:get_update_metadata(Job),
     MD2 = riakc_obj:set_secondary_index(MD1,
					[{{binary_index, "status"}, [Status]}]),       
     FinalJob = riakc_obj:update_metadata(Job, MD2),
     riakc_pb_socket:put(Pid, FinalJob).

%===================
% UPDATING STATUS
%===================
     
%resets the status and the candidates list
% @private
return_ready(Pid, KeyJob, Priority) ->
     empty_candidates(Pid, KeyJob),
     set_secondary_index(Pid, KeyJob, Priority).
     
%updates the status from ready to running
% @private
running_job(Pid, KeyJob) ->
     set_secondary_index(Pid, KeyJob, ?RUNNING).

%updates the status from ready to nodedown
% @private
nodedown_job(Pid, KeyJob) ->
	delete_job_candidates(Pid, KeyJob),
	set_secondary_index(Pid, KeyJob, ?NODEDOWN).
     
%updates from running to terminated and stores the result
% @private
terminated_job(Pid, KeyJob, Result) ->
	Res = get_job(Pid, KeyJob),
    if 	Res == {error,notfound} -> 
			Res;
     	true ->
     		{_, TimeStamp, Node, Module, Function, Args, _} = Res,
			 UpdatedJob = #job{time = TimeStamp,
						       node = Node,
						       module = Module,
						       function = Function,
						       args = Args,
						       result = Result},
			 empty_candidates(Pid, KeyJob),
			 save_job(Pid, UpdatedJob, ?TERMINATED)
	end.

%resets candidates list
% @private
empty_candidates(Pid, KeyJob) ->
	EmptyCandidates = #candidates{job = KeyJob,
		                          nodes = []},
    save_candidate(Pid, EmptyCandidates).
     	
%============
% DELETING
%============

%deletes job from the Jobs bucket, from the Candidates bucket 
%and from the job list in the bucket MyJobs
% @private
delete_job(Pid, KeyJob) ->
	riakc_pb_socket:delete(Pid, ?JOB_BUCKET, KeyJob),
	delete_job_candidates(Pid, KeyJob),
    NewListJobs = #myJobs{node = node(),
                          jobs = delete(KeyJob, list_my_jobs(Pid))},
    save_my_jobs(Pid, NewListJobs).
	
%deletes the job from the candidates bucket
% @private
delete_job_candidates(Pid, KeyJob) ->
	riakc_pb_socket:delete(Pid, ?CANDIDATE_BUCKET, KeyJob).
	
%deletes all the elements in the bucket MyJobs
% @private
empty_my_jobs(Pid) ->
    EmptyList = #myJobs{node = node(),
		                jobs = []},
    save_my_jobs(Pid, EmptyList).

%node deletes a specified job, if not running, from his own
delete_my_job(Pid, KeyJob) ->
    NodeA = node(),
	NodeB = get_job_node(Pid, KeyJob),
	if 	NodeB == {error,notfound} ->
			io:format("Job ~p doesn't exist~n", [KeyJob]);
		NodeA /= NodeB ->
			io:format("You can't delete the job ~p, it's not yours!~n", [KeyJob]),
			error;
		true ->
			delete_my_safe_job(Pid, binary(KeyJob))
	end.

%deletes a specified not running job
% @private	
delete_my_safe_job(Pid, KeyJob) ->
    Status = get_status(Pid, KeyJob),
    if  Status == ?RUNNING ->
            io:format("You can't delete the job ~p, another node is running it! ~n",[KeyJob]),
            error;
        true ->
            delete_job(Pid, KeyJob)            
    end.
        	
%Node deletes all his jobs if not running
delete_my_jobs(Pid) ->
    ListJobs = list_my_jobs(Pid),
    foreach(fun(K) -> delete_my_safe_job(Pid, K) end, ListJobs).
 
%checks if there are jobs marked as <<"nodedown">>, and restores them to <<"ready">>
check_my_jobs(Pid) ->
	ListJobs = list_my_jobs(Pid),
	foreach(fun(X) -> case get_status(Pid,X) of ?NODEDOWN -> return_ready(Pid, X, ?READY); _ -> ok end end, ListJobs).

%the node deletes all his terminated jobs       
delete_my_terminated_jobs(Pid) ->
	ListJobs = list_my_jobs(Pid),
	foreach(fun(X) -> case get_status(Pid,X) of ?TERMINATED -> delete_job(Pid, X); _ -> ok end end, ListJobs).

%asks the user if he wants reset or delete the job
% @private
reset_or_delete(Pid, KeyJob, Priority) ->
	Answer = io:fread("Do you want to delete or restore the job? d/r>","~a"),
	if 	(Answer == {ok,[d]}) -> 
			delete_job(Pid, KeyJob),
			io:format("Job ~p has just been deleted~n", [KeyJob]);
		(Answer == {ok,[r]}) ->
			return_ready(Pid, KeyJob, Priority),
			io:format("Job ~p is ~p again~n", [KeyJob, Priority]);
		true -> 
			io:format("You have to choose an option between delete the job or restore it to ~p ~n", [Priority]),
			reset_or_delete(Pid, KeyJob, Priority)
	end.	
	
%deletes all the elements in the bucket Jobs
% @private
reset_jobs(Pid) ->
    {ok, Keys} = riakc_pb_socket:list_keys(Pid,?JOB_BUCKET),
    foreach(fun(K) -> riakc_pb_socket:delete(Pid, ?JOB_BUCKET, K) end, Keys).

%deletes all the elements in the bucket
% @private
reset_my_jobs(Pid) ->
    {ok, Keys} = riakc_pb_socket:list_keys(Pid,?MY_JOB_BUCKET),
    foreach(fun(K) -> riakc_pb_socket:delete(Pid, ?MY_JOB_BUCKET, K) end, Keys).
    
%deletes all the elements in the bucket Modules
% @private
reset_modules(Pid) ->
    {ok, Keys} = riakc_pb_socket:list_keys(Pid,?MODULE_BUCKET),
    foreach(fun(K) -> riakc_pb_socket:delete(Pid, ?MODULE_BUCKET, K) end, Keys).

%deletes all the elements in the bucket candidates
% @private
reset_candidates(Pid) ->
    {ok, Keys} = riakc_pb_socket:list_keys(Pid,?CANDIDATE_BUCKET),
    foreach(fun(K) -> riakc_pb_socket:delete(Pid, ?CANDIDATE_BUCKET, K) end, Keys).
    
%deletes all the elements in all the buckets
%use it only for testing
reset_all(Pid) ->
    reset_jobs(Pid),
    reset_my_jobs(Pid),
    reset_modules(Pid),
    reset_candidates(Pid).

