-module(composite).
-export([start/0]).
-record(dir, {name, type, children}).


start() -> 
    CompositeDir1 = 
        #dir{name="parentDir", type=d, children=[
            #dir{name="subDir", type=d, children=[]},
            #dir{name="subFile", type=f, children=[]} 
        ]},

    io:format("=>CompositeEx: component - dir:~n",[]),
    printDir(CompositeDir1).
    
printDir(D) ->
    io:format("Dir=~p, type=~p~n",[D#dir.name, D#dir.type]),
    printChildren(D#dir.children).

printChildren([])              ->
    ok;
printChildren([FirstDir|Tail]) ->
    printDir(FirstDir),
    printChildren(Tail).
    
%~ =========OUT===========
%~ =>CompositeEx: component - dir:
%~ Dir="parentDir", type=d
%~ Dir="subDir", type=d
%~ Dir="subFile", type=f

