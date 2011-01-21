-module(see_crawler_sup).
-behaviour(supervisor).

-export([start_link/0,
         stop/0,
         add/0,
         add/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    ok.

add() ->
    supervisor:start_child(?MODULE, []).

add(0) ->
    ok;

add(N) ->
    add(),
    add(N - 1).

init([]) ->
    {ok, {{simple_one_for_one, 10, 5},
            [{1, {see_crawler, start_link, []}, transient, 1,
                    worker, [see_crawler]}]}}.
