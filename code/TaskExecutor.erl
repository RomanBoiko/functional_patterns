-module(functionalAdapter).
-export([start/0]).

start() -> 
    io:format("=>Call Adapter for printList function:~n",[]),
    adapter(fun printList/1, [3,14,15,92,6]),
    
    io:format("=>Call Adapter for printTuple function:~n",[]),
    adapter(fun printTuple/1, {3,14}).
    
adapter(Function, Params) ->
    Function(Params).
    
printList([])->
    io:format("~n",[]);
printList([H|T])->
    io:format("~p ",[H]),
    printList(T).
    
printTuple({Key, Value}) ->
    io:format("~p=>~p~n",[Key, Value]).
    
%~ =========OUT===========
%~ =>Call Adapter for printList function:
%~ 3 14 15 92 6
%~ =>Call Adapter for printTuple function:
%~ 3=>14
