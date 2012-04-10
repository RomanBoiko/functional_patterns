-module(abstractfactory).
-export([start/0]).

start() -> 
    Doubler  = multiplyByConstantFunctionFactory(2),
    Trippler = multiplyByConstantFunctionFactory(3),
    io:format("==MultiplyByConstantFactory==~n",[]),
    io:format("=>Calling doubler for argument 3: result=~p~n",[Doubler(3)]),
    io:format("=>Calling trippler for argument 3: result=~p~n",[Trippler(3)]),
    
    Add2 = addConstantFunctionFactory(2),
    Add3 = addConstantFunctionFactory(3),
    io:format("==AddConstantFactory==~n",[]),
    io:format("=>Calling Add2 for argument 5: result=~p~n",[Add2(5)]),
    io:format("=>Calling Add3 for argument 5: result=~p~n",[Add3(5)]).
    
    
    
abstractTwoArgumentsFunctionToOneArgumentFactory(ConstantArgument, Fun) -> 
    fun(VariableArgument) -> Fun(ConstantArgument, VariableArgument) end.
    
multiplyByConstantFunctionFactory(Multiplier) ->
    abstractTwoArgumentsFunctionToOneArgumentFactory(Multiplier, fun(X, Y)->X*Y end).
    
addConstantFunctionFactory(Constant) ->
    abstractTwoArgumentsFunctionToOneArgumentFactory(Constant, fun(X, Y)->X+Y end).

%~ =========OUT===========
%~ ==MultiplyByConstantFactory==
%~ =>Calling doubler for argument 3: result=6
%~ =>Calling trippler for argument 3: result=9
%~ ==AddConstantFactory==
%~ =>Calling Add2 for argument 5: result=7
%~ =>Calling Add3 for argument 5: result=8
