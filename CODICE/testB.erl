-module(testB).
-import(connect, [start/1, stop/0]).
-import(timer, [sleep/1]).
-import(trade, [ask_for_job/1]).
-import(dht, [add_important_job/4]).
-compile(export_all).

main(RiakNode) ->
	Pid = connect:start(RiakNode),
	sleep(3000),
	io:format("Now it will be executed: ~ndht:add_important_job(Pid, test, remove_prefix, [\"ciao\", \"ciao, come va?\"]).~n"),
	dht:add_important_job(Pid, test, remove_prefix, ["ciao", "ciao, come va?"]),
	sleep(3000),
	io:format("Now it will be executed:~nask_for_job(Pid).~n"),
	ask_for_job(Pid).
