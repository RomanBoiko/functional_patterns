-module(observer).
-export([start/0]).

start() -> 
    Observers = [
        fun observer1/1,fun observer2/1
    ],
    io:format("==Executing Observed==~n", []),
    subjectFunction1(Observers).


subjectFunction1(Observers) -> 
    io:format("==OBSERVED CALLED~n", []),
    notify(Observers, process_info(self(), current_function)).

notify([], _Observed) -> ok;
notify([Observer|RestOfObservers], Observed)->
    Observer(Observed),
    notify(RestOfObservers, Observed).

observer1(Observed)->
    {current_function, {M, F, _Arity}} = Observed,
    io:format("==>OBSERVER1: I was notified by ~p:~p~n",[M, F]).
observer2(Observed)->
    {current_function, {M, F, _Arity}} = Observed,
    io:format("==>OBSERVER2: I was notified by ~p:~p~n",[M, F]).

%~ =========OUT===========
%~ ==Executing Observed==
%~ ==OBSERVED CALLED
%~ ==>OBSERVER1: I was notified by observer:subjectFunction1
%~ ==>OBSERVER2: I was notified by observer:subjectFunction1
