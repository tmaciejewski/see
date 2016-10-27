-module(see_crawler_sup).
-behaviour(supervisor).

-export([start_link/0,
         stop/0,
         add/1,
         add/2]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    ok.

add(_, 0) ->
    ok;

add(DbNode, N) ->
    add(DbNode),
    add(DbNode, N - 1).

add(DbNode) ->
    supervisor:start_child(?MODULE, [DbNode]).

init([]) ->
    {ok, {{simple_one_for_one, 10, 5},
            [{1, {see_crawler_worker, start_link, []}, transient, 1,
                    worker, [see_crawler_worker]}]}}.
