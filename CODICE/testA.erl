-module(testA).
-import(connect, [start/1, stop/0]).
-import(timer, [sleep/1]).
-import(trade, [ask_for_job/1]).
-import(dht, [add_job/4, add_important_job/4, get_my_jobs/1, delete_my_terminated_jobs/1]).
-compile(export_all).

main(RiakNode) ->
	Pid = connect:start(RiakNode),
	sleep(3000),
	io:format("Now it will be executed: ~ndht:add_job(Pid, test, make_prime, [30]).~n"),
	dht:add_job(Pid, test, make_prime, [30]),
	sleep(3000),
	io:format("Now it will be executed:~nask_for_job(Pid).~n"),
	ask_for_job(Pid).
