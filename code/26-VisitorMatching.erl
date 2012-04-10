-module(visitorMatching).
-export([start/0]).

start() ->
    Expr = {1, '+', {2, '*', 3}},
    io:format("EvalExpr res: ~p ~n", [evaluateVisitor(Expr)]),
    io:format("PrintExpr res: ~s ~n", [printVisitor(Expr)]).

evaluateVisitor(Expr) -> evalExpr(Expr).

evalExpr({L, '+', R}) ->
    evalExpr(L)+evalExpr(R);
evalExpr({L, '*', R}) ->
    evalExpr(L)*evalExpr(R);
evalExpr(Number) ->
    Number.

printVisitor(Expr) -> printExpr(Expr).

printExpr({L, '+', R}) ->
    lists:concat(["(", printExpr(L),"+",printExpr(R), ")"]);
printExpr({L, '*', R}) ->
    lists:concat(["(", printExpr(L),"*",printExpr(R), ")"]);
printExpr(Number) ->
    lists:concat(["",Number]).

%~ =========OUT===========
%~ EvalExpr res: 7
%~ PrintExpr res: (1+(2*3))