-module(builder).
-export([start/0]).
-record(dir, {name, type, children}).


start() -> 
    Dir1BuiltWithoutChildren = withName(newDir(), "parentDir"),
    Dir1WithChildrenAdded = withChildren(Dir1BuiltWithoutChildren, [withName(newDir(), "subDir"), withName(newFile(), "subFile")]),
    io:format("=>BuilderEx: built dirresult=>~n",[]),
    printDir(Dir1WithChildrenAdded).
    
newDir() -> #dir{type=dir, children=[]}.
newFile() -> #dir{type=file, children=[]}.

withName(FSObject, Name) ->
    #dir{name=Name, type=FSObject#dir.type, children=FSObject#dir.children}.
    
withChildren(FSObject, Children) when FSObject#dir.type==dir ->
    #dir{name=FSObject#dir.name, type=FSObject#dir.type, children=Children}.

printDir(D) ->
    io:format("Dir=~p, type=~p~n",[D#dir.name, D#dir.type]),
    printChildren(D#dir.children).

printChildren([])              ->
    ok;
printChildren([FirstDir|Tail]) ->
    printDir(FirstDir),
    printChildren(Tail).
    
%~ =========OUT===========
%~ =>BuilderEx: built dirresult=>
%~ Dir="parentDir", type=dir
%~ Dir="subDir", type=dir
%~ Dir="subFile", type=file
