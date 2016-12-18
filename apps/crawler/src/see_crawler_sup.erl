-module(see_crawler_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(DEFAULT_CRAWLER_NUM, 1).

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Options).

init(Options) ->
    CrawlerNum = proplists:get_value(crawler_num, Options, ?DEFAULT_CRAWLER_NUM),
    CrawlersSpec = [make_spec(Id, Options) || Id <- lists:seq(1, CrawlerNum)],
    {ok, {{one_for_one, 10, 5}, CrawlersSpec}}.

make_spec(Id, Options) ->
    {Id, {see_crawler_worker, start_link, [Options]},
     transient, 1000, worker, [see_crawler_worker]}.
