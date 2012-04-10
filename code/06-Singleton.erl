-module(singleton_connection).
-export([start/0]).
-export([get/0, loop/1]).

start() -> 
    io:format("==GetSingletonFirstTime=>~n",[]),
    singleton_connection:get(),
    io:format("==GetSingletonSecondTime=>~n",[]),
    singleton_connection:get().

get() ->
    createSingletonIfNotCreatedYet(registered()),
    ?MODULE ! {get, self()},
    receive
        Connection ->
            io:format("~p~n",[Connection]),
            Connection
    end.
     
createSingletonIfNotCreatedYet([?MODULE|_REST_OF_REGISTERED_PROCESSES]) ->
    io:format("==>>Cached Connection returned: ",[]),
    ok;
createSingletonIfNotCreatedYet([_NOT_OUR_PROCESS|REST_OF_REGISTERED_PROCESSES]) ->
    createSingletonIfNotCreatedYet(REST_OF_REGISTERED_PROCESSES);
createSingletonIfNotCreatedYet([]) ->
    Connection = createExpensiveConnectionObject(),
    io:format("==>>Expensive Connection Created: ",[]),
    register(?MODULE, spawn(?MODULE, loop, [Connection])).
 
loop(CachedConnection) ->
    receive
        {get, From} ->
            From ! CachedConnection,
            loop(CachedConnection)
        end.

createExpensiveConnectionObject() ->
    connection_id1.

%~ =========OUT===========
%~ ==GetSingletonFirstTime=>
%~ ==>>Expensive Connection Created: connection_id1
%~ ==GetSingletonSecondTime=>
%~ ==>>Cached Connection returned: connection_id1
