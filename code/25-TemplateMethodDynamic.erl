-module(templateMethod).
-export([start/0, callTemplateMethod/2, templateVariabilityPart1/0, templateVariabilityPart2/0]).

start() -> 
    spawn(fun() -> templateInvariant() end).

templateInvariant() ->
    receive
        {Pid, F} -> 
            io:format("invariant Do before Variability part~n",[]),
            Result=F(),
            io:format("invariant Do after Variability part~n",[]),
            Pid ! {self(), Result}
    end.

callTemplateMethod(Pid, Q) ->
    Pid ! {self(), Q},
    receive
        {Pid, Reply} -> Reply
    end.

templateVariabilityPart1() ->
    io:format("ping1 executed~n",[]).

templateVariabilityPart2() ->
    io:format("ping2 executed~n",[]).

%~ =========OUT===========
%~ 1> Pid1=templateMethod:start().
%~ <0.33.0>
%~ 2> templateMethod:callTemplateMethod(Pid1, fun templateMethod:templateVariabilityPart1/0).
%~ invariant Do before Variability part
%~ ping1 executed
%~ invariant Do after Variability part
%~ ok
%~ 3> Pid2=templateMethod:start().
%~ <0.37.0>
%~ 4> templateMethod:callTemplateMethod(Pid2, fun templateMethod:templateVariabilityPart2/0).
%~ invariant Do before Variability part
%~ ping2 executed
%~ invariant Do after Variability part
%~ ok
