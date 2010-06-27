-module(crawler_sup).
-compile(export_all).

-import(crawler).

start() ->
    supervisor:start_link(?MODULE, []).

stop(Sup) ->
    Shutdown = fun({_, Pid, _, _}) -> crawler:stop(Pid) end,
    lists:foreach(Shutdown, supervisor:which_children(Sup)).

add(Sup) ->
    supervisor:start_child(Sup, []).

add(_, 0) ->
    ok;

add(Sup, N) ->
    add(Sup),
    add(Sup, N - 1).

init([]) ->
    {ok, {{simple_one_for_one, 10, 5}, [{1, {crawler, start, []}, transient, 1,
                    worker, [crawler]}]}}.
