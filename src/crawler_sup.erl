-module(crawler_sup).
-compile(export_all).

-import(crawler).

start(_Type, _Args) ->
    supervisor:start_link({global, crawler_sup}, ?MODULE, []).

start_phase(db, _Type, _Args) ->
    application:start(db),
    ok.

stop(_) ->
    Shutdown = fun({_, Pid, _, _}) -> crawler:stop(Pid) end,
    lists:foreach(Shutdown, supervisor:which_children({global, crawler_sup})).

add() ->
    supervisor:start_child({global, crawler_sup}, []).

add(0) ->
    ok;

add(N) ->
    add(),
    add(N - 1).

init([]) ->
    {ok, {{simple_one_for_one, 10, 5}, [{1, {crawler, start, []}, transient, 1,
                    worker, [crawler]}]}}.
