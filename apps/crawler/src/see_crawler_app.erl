-module(see_crawler_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(PARAM_KEYS, [db_node, crawler_num]).
-define(DEFAULT_CRAWLER_NUM, 1).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case get_params() of
        {ok, Params} ->
            CrawlerNum = proplists:get_value(crawler_num, Params),
            DbNode = proplists:get_value(db_node, Params),
            see_crawler_sup:start_link(CrawlerNum, DbNode);
        {error, ParamNotFound} ->
            {error, ParamNotFound}
    end.

get_params() ->
    Params = lists:map(fun get_param/1, ?PARAM_KEYS),
    case lists:keyfind(error, 1, Params) of
        false ->
            {ok, Params};
        ParamNotFound ->
            {error, ParamNotFound}
    end.

get_param(crawler_num) ->
    case application:get_env(crawler_num) of
        {ok, CrawlerNum} ->
            {crawler_num, CrawlerNum};
        undefined ->
            {crawler_num, ?DEFAULT_CRAWLER_NUM}
    end;

get_param(db_node) ->
    case application:get_env(db_node) of
        {ok, DbNode} ->
            {db_node, DbNode};
        undefined ->
            {error, db_node}
    end.

stop(_State) ->
    ok.
