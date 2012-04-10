-module(chain).
-export([start/0]).
-define(CAN_PROCESS, 0).
-define(CANNOT_PROCESS, 1).

start() ->
	ChainWithConditions = [fun accept1/1, fun accept2/1, fun accept3/1],
	ChainWithGuards = [fun acceptGuarded1/1, fun acceptGuarded2/1, fun acceptGuarded3/1],
	
	lists:foreach(
			fun(N)->io:format("=CHAIN with Conditions -> ARG=~B=~n", [N]),processArg(N, ChainWithConditions) end,
			[1,2,3,4]
		),
	
	lists:foreach(
			fun(N)->io:format("=CHAIN with Guards -> ARG=~B=~n", [N]),processArg(N, ChainWithGuards) end,
			[1,2,3,4]
		).
	
accept1(N) -> 
	if
		N==1 -> io:format("acc1: I can accept~n"    ), ?CAN_PROCESS;
		true -> io:format("acc1: I can not accept~n"), ?CANNOT_PROCESS
	end.
	
accept2(N) -> 
	if
		N==2 -> io:format("acc2: I can accept~n"    ), ?CAN_PROCESS;
		true -> io:format("acc2: I can not accept~n"), ?CANNOT_PROCESS
	end.
	
accept3(N) -> 
	if
		N==3 -> io:format("acc3: I can accept~n"    ), ?CAN_PROCESS;
		true -> io:format("acc3: I can not accept~n"), ?CANNOT_PROCESS
	end.

	
acceptGuarded1(N) when N==1 -> io:format("acc1: I can accept~n"    ), ?CAN_PROCESS;
acceptGuarded1(_)           -> io:format("acc1: I can not accept~n"), ?CANNOT_PROCESS.

acceptGuarded2(N) when N==2 -> io:format("acc2: I can accept~n"    ), ?CAN_PROCESS;
acceptGuarded2(_)           -> io:format("acc2: I can not accept~n"), ?CANNOT_PROCESS.
	
acceptGuarded3(N) when N==3 -> io:format("acc3: I can accept~n"    ), ?CAN_PROCESS;
acceptGuarded3(_)           -> io:format("acc3: I can not accept~n"), ?CANNOT_PROCESS.


processArg(N, [H|T]) ->
	HeadRes = H(N),
	if
		HeadRes==?CANNOT_PROCESS -> processArg(N, T);
		true                     -> ok
	end;
processArg(N, []) -> io:format("==>No handler to process arg ~B found in chain~n", [N]).

%~ =========OUT===========
%~ #########STARTING###########
%~ =CHAIN with Conditions -> ARG=1=
%~ acc1: I can accept
%~ =CHAIN with Conditions -> ARG=2=
%~ acc1: I can not accept
%~ acc2: I can accept
%~ =CHAIN with Conditions -> ARG=3=
%~ acc1: I can not accept
%~ acc2: I can not accept
%~ acc3: I can accept
%~ =CHAIN with Conditions -> ARG=4=
%~ acc1: I can not accept
%~ acc2: I can not accept
%~ acc3: I can not accept
%~ ==>No handler to process arg 4 found in chain
%~ =CHAIN with Guards -> ARG=1=
%~ acc1: I can accept
%~ =CHAIN with Guards -> ARG=2=
%~ acc1: I can not accept
%~ acc2: I can accept
%~ =CHAIN with Guards -> ARG=3=
%~ acc1: I can not accept
%~ acc2: I can not accept
%~ acc3: I can accept
%~ =CHAIN with Guards -> ARG=4=
%~ acc1: I can not accept
%~ acc2: I can not accept
%~ acc3: I can not accept
%~ ==>No handler to process arg 4 found in chain