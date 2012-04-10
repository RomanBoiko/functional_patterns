-module(templateMethod).
-export([start/0]).

start() -> 
    %Instantiate 'TEMPLATE CLASS1'
    TemplateClass1 = fun()-> templateInvariant(fun templateVariabilityPart1/0) end,
    io:format("===call 'TEMPLATE METHOD' from Object(Class1)==~n",[]),
    TemplateClass1(),

    %Instantiate 'TEMPLATE CLASS2'
    TemplateClass2 = fun()-> templateInvariant(fun templateVariabilityPart2/0) end,
    io:format("===call 'TEMPLATE METHOD' from Object(Class2)==~n",[]),
    TemplateClass2().

templateInvariant(F) ->
    io:format("invariant Do before Variability part~n",[]),
    F(),
    io:format("invariant Do after Variability part~n",[]).

templateVariabilityPart1() ->
    io:format("ping1 executed~n",[]).

templateVariabilityPart2() ->
    io:format("ping2 executed~n",[]).

%~ =========OUT===========
%~ ===call 'TEMPLATE METHOD' from Object(Class1)==
%~ invariant Do before Variability part
%~ ping1 executed
%~ invariant Do after Variability part
%~ ===call 'TEMPLATE METHOD' from Object(Class2)==
%~ invariant Do before Variability part
%~ ping2 executed
%~ invariant Do after Variability part