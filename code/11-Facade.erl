-module(facade).
-export([start/0]).

start() -> 
    List = [3,14,15,92],
    io:format("=>Call Facade to print List as list:~n",[]),
    facadeWhichHidesUnderlyingSubProcedures(as_list, List),
    
    io:format("=>Call Facade to print List as set of tuples:~n",[]),
    facadeWhichHidesUnderlyingSubProcedures(as_tuples, List).
    
facadeWhichHidesUnderlyingSubProcedures(as_list, List) ->
    printList(List);

facadeWhichHidesUnderlyingSubProcedures(as_tuples, []) ->
    ok;
    
facadeWhichHidesUnderlyingSubProcedures(as_tuples, [Key, Value|Tail]) ->
    printTuple({Key, Value}),
    facadeWhichHidesUnderlyingSubProcedures(as_tuples, Tail).
    
printList([])->
    io:format("~n",[]);
printList([H|T])->
    io:format("~p ",[H]),
    printList(T).
    
printTuple({Key, Value}) ->
    io:format("~p=>~p~n",[Key, Value]).
    
%~ =========OUT===========
%~ =>Call Facade to print List as list:
%~ 3 14 15 92
%~ =>Call Facade to print List as set of tuples:
%~ 3=>14
%~ 15=>92

