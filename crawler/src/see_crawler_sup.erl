-module(see_crawler_sup).
-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

start_link(CrawlerNum, DbNode) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [CrawlerNum, DbNode]).

init([CrawlerNum, DbNode]) ->
    CrawlersSpec = [make_spec(Id, DbNode) || Id <- lists:seq(1, CrawlerNum)],
    {ok, {{one_for_one, 10, 5}, CrawlersSpec}}.

make_spec(Id, DbNode) ->
    {Id, {see_crawler_worker, start_link, [DbNode]},
     transient, 1000, worker, [see_crawler_worker]}.
