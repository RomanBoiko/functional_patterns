-module(interpreter).
-export([start/0]).

start() -> 
    calculate("(1+(2*3))"),
    calculate("((1+2)*3)").

calculate(Expr)->
    AST = createAbstractSyntaxTree(Expr),
    CalculationResult = interpret(AST),
    
    io:format("Input: ~p~n", [Expr]),
    io:format("AST: ~p~n", [AST]),
    io:format("InterpetedExpr2: ~p ~n~n", [CalculationResult]).

createAbstractSyntaxTree(Expr) -> ast(Expr).
ast([BS, OL, A, OR, BE | _T]) when ([BS]=="(") and ([BE]==")") ->
    {LeftOperand, _}=string:to_integer([OL]),
    {RightOperand, _}=string:to_integer([OR]),
    {LeftOperand, [A], RightOperand};
ast([_BS, OL, A, OR, BE | T]) when [OR]=="(" ->
    {LeftOperand, _}=string:to_integer([OL]),
    {LeftOperand, [A], ast(lists:concat([[OR], [BE], T]))};
ast([_BS1, BS2, OL, A, OR, BE2, A2, OR2 | _T]) when [BS2]=="(" ->
    {RightOperand, _}=string:to_integer([OR2]),
    {ast(lists:concat([[BS2], [OL], [A], [OR], [BE2]])), [A2], RightOperand}.

interpret(Expr)       -> evalExpr(Expr).
evalExpr({L, "+", R}) -> evalExpr(L)+evalExpr(R);
evalExpr({L, "*", R}) -> evalExpr(L)*evalExpr(R);
evalExpr(Number)      -> Number.

%~ =========OUT===========
%~ Input: "(1+(2*3))"
%~ AST: {1,"+",{2,"*",3}}
%~ InterpetedExpr2: 7

%~ Input: "((1+2)*3)"
%~ AST: {{1,"+",2},"*",3}
%~ InterpetedExpr2: 9
