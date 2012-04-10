-module(hylomorphism).
-export([start/0]).
-record(hylomorphic_task, {
    do,
    till,
    catamorfic_fun,
    anamorfic_funs
}).

start() -> 
    io:format("==>evens(15)~n",[]),
    printList(evens(15)),
    io:format("~n==>factorial(4)~n~p~n",[factorial(4)]),
    io:format("~n==>to_binary(11)~n",[]),
    printList(to_binary(11)),
    io:format("~n==>expand([{a,2},{b,3},{c,4}])~n",[]),
    printList(expand([{'a',2},{'b',3},{'c',4}])).

printList([])   -> io:format("~n",[]);
printList([H|T])-> io:format("~p ",[H]), printList(T).

new(PL) when is_list(PL) ->
    HMTask = #hylomorphic_task{
        do              = proplists:get_value(do,             PL, fun(X) -> X + 1           end),
        till            = proplists:get_value(till,           PL, fun(X) -> X =:= undefined end),
        catamorfic_fun  = proplists:get_value(catamorfic_fun, PL, fun(X) -> X               end),
        anamorfic_funs  = proplists:get_value(anamorfic_funs, PL, { [], fun(A,E) -> [E|A] end, fun lists:reverse/1 })
    },
    fun(InputData) -> eval(HMTask, InputData, element(1,HMTask#hylomorphic_task.anamorfic_funs)) end.

eval(HMTask, Input, Result) ->
    {_InjectingFlagAtom, FormatTempFoldingResultFunction, FormatResultFunction} = HMTask#hylomorphic_task.anamorfic_funs,
    case (HMTask#hylomorphic_task.till)(Input) of
        false ->
            CatamorphicFoldResult  = (HMTask#hylomorphic_task.catamorfic_fun)(Input),
            CurrentIterationResult = (HMTask#hylomorphic_task.do)(Input),
            eval(HMTask, CurrentIterationResult, FormatTempFoldingResultFunction(Result, CatamorphicFoldResult));
        true -> FormatResultFunction(Result)
    end.

%Example Hylomorphism calculations
evens(N) ->
    HMTask = new([
        {do,   fun(X)            -> X + 2  end},
        {till, fun(X)            -> X >= N end}
    ]),
    HMTask(0).

factorial(N) when N > 0 ->
    HMTask = new([
        {do,   fun(X)            -> X - 1  end},
        {till, fun(X)            -> X =< 1 end},
        {anamorfic_funs, {1, fun(A,E) -> A * E end, fun(A) -> A end}}
    ]),
    HMTask(N).

to_binary(N) when N > 0 ->
    HMTask = new([
        {do,             fun(X) -> X div 2   end},
        {till,           fun(X) -> X =< 0    end},
        {catamorfic_fun, fun(X) -> (X rem 2) end},
        {anamorfic_funs,  {[], fun(A,E) -> [E|A] end, fun(A) -> A end}}
    ]),
    HMTask(N).

expand(L) ->
    HMTask = new([
        {do, fun ([_|T]) -> T; ([])  -> []    end},
        {till, fun ([]) -> true; (_) -> false end},
        {catamorfic_fun, fun([{C,N}|_])  -> lists:duplicate(N, C) end}
    ]),
    HMTask(L).

%~ =========OUT===========
%~ ==>evens(15)
%~ 0 2 4 6 8 10 12 14

%~ ==>factorial(4)
%~ 24

%~ ==>to_binary(11)
%~ 1 0 1 1

%~ ==>expand([{a,2},{b,3},{c,4}])
%~ [a,a] [b,b,b] [c,c,c,c]
