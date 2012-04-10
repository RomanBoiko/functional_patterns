-module(state).
-export([start/0]).

start() -> 
    io:format("==>I am client and I am starting to operate with Snack machine~n",[]),
    doWhatMachineSaysToDo(start).

doWhatMachineSaysToDo(ok) ->
    io:format("Machine can be used by someone else now",[]);

doWhatMachineSaysToDo(Action) ->
    io:format("==>I am client and I doing action machine asks me: ~p~n",[Action]),
    NextAction = operateWithSnackMachine(Action),
    doWhatMachineSaysToDo(NextAction).

operateWithSnackMachine(start)           -> put_coins;
operateWithSnackMachine(put_coins)       -> select_snack;
operateWithSnackMachine(select_snack)    -> take_your_snack;
operateWithSnackMachine(take_your_snack) -> get_out;
operateWithSnackMachine(get_out)         -> ok.

%~ =========OUT===========
%~ ==>I am client and I am starting to operate with Snack machine
%~ ==>I am client and I doing action machine asks me: start
%~ ==>I am client and I doing action machine asks me: put_coins
%~ ==>I am client and I doing action machine asks me: select_snack
%~ ==>I am client and I doing action machine asks me: take_your_snack
%~ ==>I am client and I doing action machine asks me: get_out
%~ Machine can be used by someone else now
