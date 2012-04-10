-module(functionalAdapter).
-export([start/0]).

start() -> 
    List = [3,14,15,92],
    io:format("=>We have List, so we call function which accepts lists without adapter:~n",[]),
    printList(List),
    
    io:format("=>We use Adapter if we have function which accepts tuples but we want to pass same List:~n",[]),
    tuplesInputAdapter(fun printTuple/1, List).
    
tuplesInputAdapter(_Function, []) ->
    ok;
    
tuplesInputAdapter(Function, [Key, Value|Tail]) ->
    Function({Key, Value}),
    tuplesInputAdapter(Function, Tail).
    
printList([])->
    io:format("~n",[]);
printList([H|T])->
    io:format("~p ",[H]),
    printList(T).
    
printTuple({Key, Value}) ->
    io:format("~p=>~p~n",[Key, Value]).
    
%~ =========OUT===========
%~ =>We have List, so we call function which accepts lists without adapter:
%~ 3 14 15 92
%~ =>We use Adapter if we have function which accepts tuples but we want to pass same List:
%~ 3=>14
%~ 15=>92

