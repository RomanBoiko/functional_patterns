-module(decorator).
-export([start/0]).

start() -> 
    List = [3,14,15,92,6],
    io:format("==>INPUT LIST:~n",[]),
    printList(List),
    
    io:format("==Simple Decorated Invocation - adds 3 to each element in list==~n",[]),
    printList(decorated(List)),
    
    io:format("==Simple Decorator Invocation - adds 1 to each element of Decorated invocation==~n",[]),
    printList(decorator(List, fun decorated/1)).
    
decorated(List) ->
    lists:map(fun(X) -> X+3 end, List).
    
decorator(List, Decorated) ->
    ResultOfDecoratedInvocation = Decorated(List),
    lists:map(fun(X) -> X+1 end, ResultOfDecoratedInvocation).
    
printList([])->
    io:format("~n",[]);
printList([H|T])-> 
    io:format("~p ",[H]),
    printList(T).

%~ =========OUT===========
%~ ==>INPUT LIST:
%~ 3 14 15 92 6
%~ ==Simple Decorated Invocation - adds 3 to each element in list==
%~ 6 17 18 95 9
%~ ==Simple Decorator Invocation - adds 1 to each element of Decorated invocation==
%~ 7 18 19 96 10
