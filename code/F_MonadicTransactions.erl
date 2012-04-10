-module(monadic_transactions).
-export([start/0]).

start() -> 
    io:format("==>TransactionSuccessful - res:~p~n",[transactionalInvocation(4)]),
    io:format("==>TransactionFailed     - res:~p~n",[transactionalInvocation(-1)]).

transactionalInvocation(In) ->
    with(In, [
        fun(X)->{ok, X-1} end,
        fun(X)->{ok, X+2} end,
        fun(X)-> if X==0->{error, '/0'}; true->{ok, 15/X} end end,
        fun(X)->{ok, X*3} end
    ]).

with(In, [])               -> In;
with(In, [Fun|RestofFuns]) ->
    case Fun(In) of
        {ok   , Res} -> with(Res, RestofFuns);
        {error, Err} -> {error, Err}
    end.

%~ =========OUT===========
%~ ==>TransactionSuccessful - res:9.0
%~ ==>TransactionFailed     - res:{error,'/0'}
