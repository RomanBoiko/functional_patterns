-module(iterator).
-export([start/0]).

start() -> 
    List = [3,14,15,92,6],
    
    io:format("==Simple Iterator==~n",[]),
    simpleIterator(List),
    
    io:format("~n==Iterator Which Stops When Number Of Left Elements Is N==~n",[]),
    iteratorWhichStopsWhenNLeft(List, 2),
    io:format("~n==Same over empty list==~n",[]),
    iteratorWhichStopsWhenNLeft([], 2),
    
    Tree = {1, {3, 4, {5, 8, 9}}, 3},
    io:format("~n==Tree Iterator==~n",[]),
    treeIterator(Tree).
    
simpleIterator([]) ->
    io:format("==>Iteration finished~n",[]);
    
simpleIterator([Head|Tail]) ->
    io:format("currentElem: ~p~n", [Head]),
    simpleIterator(Tail).
    
    
iteratorWhichStopsWhenNLeft(List, N) when length(List)=<N -> 
    io:format("==>Iteration finished~n",[]);
iteratorWhichStopsWhenNLeft([Head|Tail], N)               ->
    io:format("currentElem: ~p~n", [Head]),
    iteratorWhichStopsWhenNLeft(Tail, N).

treeIterator({P, L, R}) ->
    io:format("ParentElem: ~p~n", [P]),
    treeIterator(L),
    treeIterator(R);
    
treeIterator(Leaf) ->
    io:format("Leaf: ~p~n", [Leaf]).

%~ =========OUT===========
%~ ==Simple Iterator==
%~ currentElem: 3
%~ currentElem: 14
%~ currentElem: 15
%~ currentElem: 92
%~ currentElem: 6
%~ ==>Iteration finished

%~ ==Iterator Which Stops When Number Of Left Elements Is N==
%~ currentElem: 3
%~ currentElem: 14
%~ currentElem: 15
%~ ==>Iteration finished

%~ ==Same over empty list==
%~ ==>Iteration finished

%~ ==Tree Iterator==
%~ ParentElem: 1
%~ ParentElem: 3
%~ Leaf: 4
%~ ParentElem: 5
%~ Leaf: 8
%~ Leaf: 9
%~ Leaf: 3
