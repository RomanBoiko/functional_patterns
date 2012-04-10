-module(null_object).
-export([start/0]).
-record(nameDataObject, {name}).
-define(NULL_NAME_DATA, #nameDataObject{name=null}).

start() -> 
    io:format("==Real NameDataObject==~n",[]),
    printNameDataObject(createRealObject()),
    io:format("==Predefined Null NameDataObject==~n",[]),
    printNameDataObject(createNullObject()),
    io:format("==Null returned - should throw exception==~n",[]),
    printNameDataObject(createNull()).

printNameDataObject(NameData) ->
    io:format("==>NameData.name=~p~n",[NameData#nameDataObject.name]).

createRealObject()-> #nameDataObject{name="someName"}.
createNullObject()-> ?NULL_NAME_DATA.
createNull()      -> null.


%~ =========OUT===========
%~ ==Real NameDataObject==
%~ ==>NameData.name="someName"
%~ ==Predefined Null NameDataObject==
%~ ==>NameData.name=null
%~ ==Null returned - should throw exception==
%~ {"init terminating in do_boot",{{badrecord,nameDataObject},[{proxy,printNameDataObject,1,[{file,"c:/UBS/Dev/ws/mt/code/proxy.erl"},{line,14}]},{init,start_it,1,[{file,"init.erl"},{line,1041}]},{init,start_em,1,[{file,"init.erl"},{line,1022}]}]}}