-module(command).
-export([start/0]).

start() ->
	lists:foreach(fun(F)->F() end, [fun command1/0, fun command2/0, fun command3/0]).

command1() ->
	io:format("executing command1~n").
	
command2() ->
	io:format("executing command2~n").
	
command3() ->
	io:format("executing command3~n").
	
%~ =========OUT===========
%~ executing command1
%~ executing command2
%~ executing command3