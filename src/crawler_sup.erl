-module(crawler_sup).
-compile(export_all).

-import(crawler).

start() ->
    supervisor:start_link(?MODULE, []).

stop(Sup) ->
    Shutdown = fun({_, Pid, _, _}) -> crawler:stop(Pid) end,
    lists:map(Shutdown, supervisor:which_children(Sup)),
    ok.

init([]) ->
    {ok, {{simple_one_for_one, 1, 1}, [{1, {crawler, start, []}, permanent, 1, 
                    worker, [crawler]}]}}.
