%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(test).
-import(lists, [reverse/1, any/2,  map/2, all/2]).
-export([make_prime/1, is_prime/1, make_random_int/1, longest_common_prefix/1, remove_prefix/2]).

make_prime(1) -> %% <label id="make_primes1" />
    lists:nth(random:uniform(4), [2,3,5,7]);
make_prime(K) when K > 0 -> %% <label id="make_primes2" />
    new_seed(),
    N = make_random_int(K),
    if N > 3 ->
	    io:format("Generating a ~w digit prime ",[K]),
	    MaxTries = N - 3,
	    P1 = make_prime(MaxTries, N+1), 
	    io:format("~n",[]),
	    P1;
	true ->
	    make_prime(K)
    end. %% <label id="make_primes3" />

make_prime(0, _) ->    %% <label id="prime_loop1" />
    exit(impossible);
make_prime(K, P) ->
    %io:format(".",[]),
    case is_prime(P) of
	true  -> P;
	false -> make_prime(K-1, P+1)
    end. %% <label id="prime_loop2" />

is_prime(D) when D < 10 ->
    lists:member(D, [2,3,5,7]);
is_prime(D) ->
    new_seed(),
    is_prime(D, 100).

is_prime(D, Ntests) ->
    N = length(integer_to_list(D)) -1,
    is_prime(Ntests, D, N).

is_prime(0, _, _) -> true;  %% <label id="is_prime_1" />
is_prime(Ntest, N, Len) ->  %% <label id="is_prime_2" />
    K = random:uniform(Len),
    %% A is a random number less than K 
    A = make_random_int(K),
    if 
	A < N ->
	    case pow(A,N,N) of
		A -> is_prime(Ntest-1,N,Len);
		_ -> false
	    end;
	true ->
	    is_prime(Ntest, N, Len)
    end. %% <label id="is_prime_3" />

%% make_random_int(N) -> a random integer with N digits.
make_random_int(N) -> new_seed(), make_random_int(N, 0). %% <label id="make_ran_int1" /> 

make_random_int(0, D) -> D;
make_random_int(N, D) ->
    make_random_int(N-1, D*10 + (random:uniform(10)-1)). %% <label id="make_ran_int2" /> 



%END:make_ran_int

new_seed() ->
    {_,_,X} = erlang:now(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).


pow(A, 1, M) ->
    A rem M;
pow(A, 2, M) ->
    A*A rem M;
pow(A, B, M) ->
    B1 = B div 2,
    B2 = B - B1,
    %% B2 = B1 or B1+1
    P = pow(A, B1, M),
    case B2 of
	B1 -> (P*P) rem M;
	_  -> (P*P*A) rem M
    end.

%% remove_prefix(X, Y) -> Z
%%   finds Z such that X ++ Z = Y
%%   
remove_prefix([H|T], [H|T1]) -> remove_prefix(T, T1);
remove_prefix([], L)         -> L.

%% longest_common_prefix([str()]) -> str()

longest_common_prefix(L) ->
    longest_common_prefix(L, []).

longest_common_prefix(Ls, L) ->
    case have_common_prefix(Ls) of
	{yes, H, Ls1} ->
	    longest_common_prefix(Ls1, [H|L]);
	no ->
	    reverse(L)
    end.

have_common_prefix([]) -> no;
have_common_prefix(L) ->
    case any(fun is_empty_list/1, L) of
	true  -> no;
	false ->
	    %% All lists have heads and tails
	    Heads = map(fun(I) -> hd(I) end, L),
	    H = hd(Heads),
	    case all(fun(X) -> hd(X) =:= H end, L) of
		true -> 
		    Tails = map(fun(I) -> tl(I) end, L),
		    {yes, H, Tails};
		false ->
		    no
	    end
    end.


is_empty_list([]) ->	true;
is_empty_list(X) when is_list(X) -> false.

