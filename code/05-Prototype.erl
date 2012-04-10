-module(prototype).
-export([start/0]).
-record(lecturer, {name, middlename}).

start() -> 
    Prototype=prototypeTeacherFromMultiMediaDepartment(),
    io:format("==Teacher from MultiMedia Department - middlename=~p~n",[Prototype#lecturer.middlename]).
prototypeTeacherFromMultiMediaDepartment()-> #lecturer{middlename="Oleksandrovych"}.

%~ =========OUT===========
%~ ==Teacher from MultiMedia Department - middlename="Oleksandrovych"
