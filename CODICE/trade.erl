-module(trade). 

-import(dht, [get_job/2, running_job/2, terminated_job/3, get_candidates/2, add_candidate/3, nodedown_job/2, search_oldest_job/1]).    
-import(aux, [performance/0, load_beam/2, ask_again_for_job/1]).
-export([ask_for_job/1]).
-include("dht.hrl").


%asks for the oldest job
%catturare eccezione con try catch
ask_for_job(Pid) -> 
	try search_oldest_job(Pid) of
		[KeyJob, Priority] ->
			{if KeyJob == notfound -> io:format("There aren't any ready jobs! ~n",[]);
				true -> 
					spawn(fun() -> submit_for_job(Pid, KeyJob, Priority) end)
			end}
	catch
		_:_ -> 
			{io:format("An exception occurs. ~nMaybe you have to restore your Pid in another variable with:~nPid2=connect:pid_link(RiakNode).~nor to check the Riak cluster state at http://localhost:8098/admin~n")}
	end.

%node submits to the worker list with current performance
%checks if the job is still in the bucket
%if it's not, the user can ask for another job
% @private
submit_for_job(Pid, KeyJob, Priority) ->
	io:format("Submit for job ~p ~n", [KeyJob]),
	RJob = get_job(Pid, KeyJob),
    if  RJob == {error,notfound} ->
            io:format("The job ~p is no more in the bucket. ~n",[KeyJob]);
        true ->
            Performance = performance(),
	        io:format("My performance is ~p ~n", [Performance]),
        	To = element(3, RJob),
	        add_candidate(Pid, KeyJob, Performance),
	        verify_chosen(Pid, To, KeyJob, Priority)
    end,
    ask_again_for_job(Pid).

%worker checks the status of the node submitting the job
%if the node crashes, the worker asks for another job
% @private
verify_chosen(Pid, To, KeyJob, Priority) ->
	{_, KeyJob, Candidates} = get_candidates(Pid, KeyJob),
	if 	length(Candidates) == 1 ->
			{_, Chosen} = first_submit(To, KeyJob);
		true ->
			{_, Chosen} = submit(To, KeyJob)
	end,
	%checks if the node is down before choosing the candidate
	%if the node is down, updates the status from ready to nodedown
	if 	Chosen == nodedown ->
			io:format("Node that submitted the job ~p is down. ~n", [KeyJob]),
			nodedown_job(Pid, KeyJob);
        Chosen == none ->
        	io:format("The job ~p is no more in the bucket. ~n",[KeyJob]);
		true ->
			io:format("The chosen node for job ~p is ~p ~n", [KeyJob, Chosen]),
			request_job(Pid, To, KeyJob, Chosen, Priority)
	end.

%worker, if it's the chosen one, runs the job    
%if an exception occurs, notifies the node which submitted the job
% @private
request_job(Pid, To, KeyJob, Chosen, Priority) ->
	Res = Chosen == node(),
	if  Res ->
            RJob = get_job(Pid, KeyJob),
            if  RJob == {error,notfound} ->
                    io:format("The job ~p no more in the bucket. ~n",[KeyJob]);
                true ->  
                	PidWorker = binary_to_atom(KeyJob,latin1),
					global:register_name(PidWorker, self()),
			        try execute_job(Pid, KeyJob, RJob, Priority) of 
			        _ -> 
				        {io:format("Result sent ~n")}
			        catch
				        _:_ -> 	
					        {io:format("Job ~p not terminated ~n", [KeyJob]),
					        deliver_exception(To, KeyJob, Priority)}
			        end,
					global:unregister_name(PidWorker)
			        
            end;
       	true ->
       	    io:format("I'm not the chosen worker ~n")
        
	end.

%=============
% RUNNING JOB
%=============

%worker retrieves the informations to run the job and notifies the node which submitted the job
%worker changes status to running
%notifies it's running the job
%runs the job
%sends the result 
%updates the job in the dht with the new state and the result
% @private

execute_job(Pid, KeyJob, RJob, Priority) ->
	running_job(Pid, KeyJob),
	To = element(3, RJob),
	Module = element(4, RJob),
	Function = element(5, RJob),
	Args = element(6, RJob),
	notify_running(To, KeyJob, Priority, self()),
	load_beam(Pid, Module),
	io:format("Loaded module ~p ~n", [Module]),
	io:format("If you want to interrupt the job, type:~naux:interrupt_running(~p).~n",[KeyJob]),
	Result = apply(Module, Function, Args),
	io:format("Terminated job ~p:~p~p with result ~p ~n", [Module, Function, Args, Result]),
	notify_terminated(To, KeyJob, Result),
	terminated_job(Pid, KeyJob, Result).


%=================
% REMOTE CALLS
%=================


%first node submits for job
% @private 
first_submit(Node, KeyJob) ->
	rpc:call(Node, connect, first_apply, [KeyJob]).

%notifies for job
% @private 
submit(Node, KeyJob) ->
	rpc:call(Node, connect, apply, [KeyJob]).	
	
%notifies that job is running
% @private 
notify_running(Node, KeyJob, Priority, PidWorker) ->
	From = node(),
	rpc:call(Node, connect, running, [KeyJob, From, Priority, PidWorker]).

%notifies that job is terminated
% @private 
notify_terminated(Node, KeyJob, Result) ->
	From = node(),
	rpc:call(Node, connect, terminated, [KeyJob, Result, From]).
	
%notifies that an exception occurred
% @private 
deliver_exception(Node, KeyJob, Priority) ->
	From = node(),
	rpc:call(Node, connect, exception, [KeyJob, From, Priority]).

