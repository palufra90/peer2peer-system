-module(connect).
-behaviour(gen_server).

-import(dht, [check_my_jobs/1, reset_or_delete/3]).
-import(aux, [start_os_mon/0, choose_worker/2, monitor_worker/4]).
-import(timer, [sleep/1]).
-import(string, [tokens/2]).
-export([start/1, stop/0, pid_link/1]).
-export([first_apply/1, apply/1, running/4, terminated/3, exception/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%-compile(export_all).
-include("dht.hrl").
-define(SERVER, ?MODULE).


%connects the client to the Riak node and waits for messages
%if some of its jobs are <<"nodedown">>, they are changed to <<"ready">>
start(RiakNode) -> 
	Pid = pid_link(RiakNode),
	gen_server:start_link({local, ?SERVER}, ?MODULE, Pid, []),
	start_os_mon(),
	net_adm:ping(RiakNode),
    check_my_jobs(Pid),
    io:format("~n~n~nWelcome!~n~nTo add a job, type: ~ndht:add_job(Pid, Module, Function, Args).~nor, if your job is important, type: ~ndht:add_important_job(Pid, Module, Function, Args).~n~nTo see if there are executable jobs, type: ~ntrade:ask_for_job(Pid).~n~nIf you forgot to store your Pid, store it with:~nPid=connect:pid_link(~p).~n~nIf an exception occurs you'll have to restore your Pid in another variable with:~nPid2=connect:pid_link(~p).~n~nIf the known RiakNode crashes, with nodes() you can see other nodes. ~nConnect to a new RiakNode with:~nPid2=connect:start(NewRiakNode).~n~n", [RiakNode, RiakNode]),
	Pid. 
	
%terminates the server
stop() -> gen_server:call(?MODULE, stop).

%connects client to the Riak node
pid_link(RiakNode) -> 
	[_, Node] = tokens(atom_to_list(RiakNode), "@"),
	{ok, Pid} = riakc_pb_socket:start_link(Node, 8087),
	Pid.

%=========================
%CALLS TO THE GEN_SERVER
%=========================

%the first candidate applies to execute the job
first_apply(KeyJob) -> gen_server:call(?MODULE, {first_apply, KeyJob}, 15000).

%other candidates apply to execute the job
apply(KeyJob) -> gen_server:call(?MODULE, {apply, KeyJob}, 15000).

%worker notifies that the job is running
running(KeyJob, Runner, Priority, PidWorker) -> gen_server:call(?MODULE, {running, KeyJob, Runner, Priority, PidWorker}).

%worker notifies that the job is terminated
terminated(KeyJob, Result, Terminator) -> gen_server:call(?MODULE, {terminated, KeyJob, Result, Terminator}).

%worker notifies that an exception occurs
exception(KeyJob, Runner, Priority) -> gen_server:call(?MODULE, {exception, KeyJob, Runner, Priority}).

%initilizes the state with the Pid
% @private
init(Pid) -> {ok, Pid}.
    

%=============================================
% CALLBACK OF THE NODE THAT SUBMITTED THE JOB
%=============================================

% @private 

%after ten seconds since the first candidate, the node chooses the best worker
handle_call({first_apply, KeyJob}, _From, Pid) ->
	io:format("Nodes start applying for job ~p ~n", [KeyJob]),
	sleep(10000),
	Chosen = choose_worker(Pid, KeyJob),
	io:format("Node chosen for job ~p is ~p ~n", [KeyJob, Chosen]),
	Reply = {chosen_worker, Chosen},
	{reply, Reply, Pid};

%the node replies to the other candidates
handle_call({apply, KeyJob}, _From, Pid) ->
	io:format("Another node applying for job ~p ~n", [KeyJob]),
	Chosen = choose_worker(Pid, KeyJob),
	Reply = {chosen_worker, Chosen},
	{reply, Reply, Pid};

%job is running, the node starts monitoring the worker
%if the worker crashes, the job returns to the original state
handle_call({running, KeyJob, Runner, Priority, PidWorker}, _From, Pid) ->
	io:format("Node ~p, running job: ~p ~nIf you want to interrupt the job, type:~naux:interrupt_runner(Pid, ~p, ~p, ~p).~n", [Runner, KeyJob, KeyJob, Priority, Runner]),
	spawn(fun() -> monitor_worker(PidWorker, Pid, KeyJob, Priority) end),
    Reply =  {Runner, running, KeyJob},
    {reply, Reply, Pid};
 
%job is terminated, the node receives the result 
handle_call({terminated, KeyJob, Result, Terminator}, _From, Pid) ->
	io:format("Job ~p terminated by ~p, the result is: ~p ~nTo delete it, type:~ndht:delete_my_job(Pid,~p).~nTo delete all your terminated jobs, type:~ndht:delete_my_terminated_jobs(Pid).~n", [KeyJob, Terminator, Result,KeyJob]),
    Reply =  {Terminator, terminated, KeyJob, with_result, Result},
    {reply, Reply, Pid};
  
%an exception occurs during the execution
%the job has returned to the original state
handle_call({exception, KeyJob, Runner, Priority}, _From, Pid) ->
	io:format("Node ~p can't terminate the job ~p. ~n", [Runner, KeyJob]),
	reset_or_delete(Pid, KeyJob, Priority),
	Reply = {exception_delivered, KeyJob},
	{reply, Reply, Pid};

  
handle_call(stop, _From, Pid) ->
    {stop, normal, stopped, Pid}.
% @private 
handle_cast(_Msg, State) -> {noreply, State}.
% @private 
handle_info(_Info, State) -> {noreply, State}.
% @private 
terminate(_Reason, _State) -> ok.
% @private 
code_change(_OldVsn, State, _Extra) -> {ok, State}.

