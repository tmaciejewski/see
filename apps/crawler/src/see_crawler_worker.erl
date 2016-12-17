-module(see_crawler_worker).
-behaviour(gen_server).

-export([start_link/1,
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SLEEP_TIMEOUT, 3000).

start_link(DbNode) ->
    gen_server:start_link(?MODULE, DbNode, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%----------------------------------------------------------

init(Args) ->
    {ok, Args, ?SLEEP_TIMEOUT}.

handle_info(timeout, DbNode) ->
    case rpc:call(DbNode, see_db_srv, next, []) of
        nothing ->
            error_logger:info_msg("Nothing to do"),
            {noreply, DbNode, ?SLEEP_TIMEOUT};
        {ok, Next} ->
            error_logger:info_msg("Visiting " ++ Next),
            visit(DbNode, Next),
            {noreply, DbNode, ?SLEEP_TIMEOUT};
        {badrpc, Reason} ->
            error_logger:error_report([{badrpc, Reason}, {node, DbNode}]),
            {noreply, DbNode, ?SLEEP_TIMEOUT}
   end.


handle_call(_, _, DbNode) ->
    {reply, ok, DbNode}.

handle_cast(stop, DbNode) ->
    {stop, normal, DbNode}.

terminate(_, _) ->
    ok.
    
code_change(_OldVsn, DbNode, _) ->
    {ok, DbNode}.

%----------------------------------------------------------

visit(DbNode, URL) ->
    case see_http:get_page(URL) of
        {ok, Content, Links} ->
            error_logger:info_report([{url, URL}, {data_length, length(Content)}, {links, length(Links)}]),
            rpc:cast(DbNode, see_db_srv, visited, [URL, {data, Content}]),
            lists:foreach(fun(Link) -> rpc:cast(DbNode, see_db_srv, queue, [Link]) end, Links);
        binary ->
            rpc:cast(DbNode, see_db_srv, visited, [URL, binary]);
        {redirect, RedirectURL} ->
            error_logger:info_report([{url, URL}, {redirect, RedirectURL}]),
            rpc:cast(DbNode, see_db_srv, visited, [URL, {redirect, RedirectURL}]),
            rpc:cast(DbNode, see_db_srv, queue, [RedirectURL]);
        {error, Reason} ->
            error_logger:error_report([{url, URL}, {error, Reason}]),
            rpc:cast(DbNode, see_db_srv, visited, [URL, {error, Reason}])
    end.
