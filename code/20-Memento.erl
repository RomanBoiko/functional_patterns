-module(memento).
-export([start/0]).

start() -> 
    io:format("==>I am client and I am starting to operate with Snack machine~n",[]),
    doWhatMachineSaysToDo(start,[]).

doWhatMachineSaysToDo(ok, History) ->
    io:format("Machine stopping, full history stack: ",[]), printList(History);

doWhatMachineSaysToDo(Action, History) when length(History)==3 ->
    [LastAction|_Tail]= History,
    io:format("==>I am client and I want to redo previous action: ~p, when machine asks me to do: ~p~n",[LastAction,Action]),
    NextAction = operateWithSnackMachine(LastAction),
    doWhatMachineSaysToDo(NextAction, lists:concat([[LastAction], History]));

doWhatMachineSaysToDo(Action, History) ->
    io:format("==>I am client and I doing action machine asks me: ~p~n",[Action]),
    NextAction = operateWithSnackMachine(Action),
    doWhatMachineSaysToDo(NextAction, lists:concat([[Action], History])).

operateWithSnackMachine(start)           -> put_coins;
operateWithSnackMachine(put_coins)       -> select_snack;
operateWithSnackMachine(select_snack)    -> take_your_snack;
operateWithSnackMachine(take_your_snack) -> get_out;
operateWithSnackMachine(get_out)         -> ok.

printList([])   -> io:format("~n",[]);
printList([H|T])-> io:format("~p ",[H]), printList(T).

%~ =========OUT===========
%~ ==>I am client and I am starting to operate with Snack machine
%~ ==>I am client and I doing action machine asks me: start
%~ ==>I am client and I doing action machine asks me: put_coins
%~ ==>I am client and I doing action machine asks me: select_snack
%~ ==>I am client and I want to redo previous action: select_snack, when machine asks me to do: take_your_snack
%~ ==>I am client and I doing action machine asks me: take_your_snack
%~ ==>I am client and I doing action machine asks me: get_out
%~ Machine stopping, full history stack: get_out take_your_snack select_snack select_snack put_coins start

