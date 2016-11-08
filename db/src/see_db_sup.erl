-module(see_db_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok,
        {{one_for_one, 10, 1},
        [
            {see_db_srv, {see_db_srv, start_link, []}, permanent, 1000, worker, [see_db_srv]},
            {see_web, {see_web, start_link, []}, permanent, 1000, worker, [see_web]}
        ]}
    }.
