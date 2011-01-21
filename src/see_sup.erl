-module(see_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok,
        {{one_for_one, 10, 1},
        [
            {see_crawler_sup, {see_crawler_sup, start_link, []}, permanent, 1000, supervisor, [crawler_sup, crawler]},
            {see_db, {see_db, start_link, []}, permanent, 1000, worker, [see_db]}
        ]}
    }.
