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

-define(CODE_OK, 200).
-define(CODE_MOVED, 301).
-define(CODE_FOUND, 302).

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
            error_logger:error_report({badrpc, Reason}),
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

get_url(URL) ->
    case httpc:request(get, {URL, []}, [{autoredirect, false}], []) of
        {ok, {{_, ?CODE_OK, _}, Headers, Content}} ->
            case see_html:is_text(Headers) of
                true ->
                    {ok, Content};
                false ->
                    binary
            end;
        {ok, {{_, ?CODE_MOVED, _}, Headers, _}} ->
            {redirect, proplists:get_value("location", Headers)};
        {ok, {{_, ?CODE_FOUND, _}, Headers, _}} ->
            {redirect, proplists:get_value("location", Headers)};
        {ok, {{_, Code, _}, Headers, Content}} ->
            {error, {Code, Headers, Content}};
        {error, Reason} ->
            {error, Reason}
    end.

visit(DbNode, URL) ->
    case get_url(URL) of
        {ok, Content} ->
            Words = see_html:words(Content),
            Links = see_html:links(URL, Content),
            error_logger:info_report([{url, URL}, {links, length(Links)}]),
            rpc:cast(DbNode, see_db_srv, visited, [URL, Words]),
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
