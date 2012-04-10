-module(proxy).
-export([start/0]).

start() ->
	io:format("====>expensiveMethod WhenNull~n"),	
	
	io:format("====>MethodResFor1=~p~n",[ 
		expensive_when_null(someFunction(1), expensiveMethod(1), expensiveMethod(2))
	]),
	
	io:format("====>MethodResFor2=~p~n",[
		expensive_when_null(someFunction(2), expensiveMethod(1), expensiveMethod(2))
	]),
	
	io:format("~n====>proxyMethod WhenNull~n"),
	
	io:format("====>MethodResFor1=~p~n",[
		proxy_when_null(someFunction(1), fun()->expensiveMethod(1) end, fun()->expensiveMethod(2) end)
	]),
	io:format("====>MethodResFor2=~p~n",[
		proxy_when_null(someFunction(2), fun()->expensiveMethod(1) end, fun()->expensiveMethod(2) end)
	]).

	

someFunction(N) when N==1 -> null;
someFunction(_N)          -> 1.
	
	
expensiveMethod(N) ->
	io:format("==>expensiveMethod with input ~B~n", [N]), N.
	
expensive_when_null(null, B, _) -> B;
expensive_when_null(_, _, C) -> C.


proxy_when_null(null, B, _) -> B();
proxy_when_null(_, _, C) -> C().


%~ =========OUT===========
%~ #########STARTING###########
%~ ====>expensiveMethod WhenNull
%~ ==>expensiveMethod with input 1
%~ ==>expensiveMethod with input 2
%~ ====>MethodResFor1=1
%~ ==>expensiveMethod with input 1
%~ ==>expensiveMethod with input 2
%~ ====>MethodResFor2=2

%~ ====>proxyMethod WhenNull
%~ ==>expensiveMethod with input 1
%~ ====>MethodResFor1=1
%~ ==>expensiveMethod with input 2
%~ ====>MethodResFor2=2