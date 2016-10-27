-module(see_crawler_sup).
-behaviour(supervisor).

-export([start_link/2,
         add/1]).

-export([init/1]).

start_link(CrawlerNum, DbNode) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} ->
            lists:foreach(fun(_) -> add(DbNode) end, lists:seq(1, CrawlerNum)),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

add(DbNode) ->
    supervisor:start_child(?MODULE, [DbNode]).

init([]) ->
    {ok, {{simple_one_for_one, 10, 5},
            [{1, {see_crawler_worker, start_link, []}, transient, 1,
                    worker, [see_crawler_worker]}]}}.
