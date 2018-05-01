-module(see_crawler_worker).
-behaviour(gen_server).

-include_lib("hackney/include/hackney_lib.hrl").

-export([start_link/1,
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {timeout, db_node}).

-define(IDLE_TIMEOUT, 3000).
-define(BUSY_TIMEOUT, 100).

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%----------------------------------------------------------

init(Options) ->
    DbNode = proplists:get_value(db_node, Options),
    {ok, #state{db_node = DbNode}, ?BUSY_TIMEOUT}.

handle_info(timeout, State) ->
    DbNode = State#state.db_node,
    case see_db_proxy:next(DbNode) of
        nothing ->
            error_logger:info_msg("Nothing to do"),
            {noreply, State, ?IDLE_TIMEOUT};
        {ok, Next} ->
            error_logger:info_msg("Visiting ~ts", [Next]),
            visit(DbNode, Next),
            {noreply, State, ?BUSY_TIMEOUT};
        {badrpc, Reason} ->
            error_logger:error_report([{badrpc, Reason}, {node, DbNode}]),
            {noreply, State, ?IDLE_TIMEOUT}
   end.


handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_, _) ->
    ok.
    
code_change(_OldVsn, State, _) ->
    {ok, State}.

%----------------------------------------------------------

visit(DbNode, URL) ->
    case see_http:get_page(URL) of
        {ok, Content} ->
            error_logger:info_msg("Got page~n"),
            Page = see_html:parse(Content),
            Title = see_html:title(Page),
            Text = see_html:text(Page),
            Links = [absolute_link(URL, Link) || Link <- see_html:links(Page)],
            error_logger:info_report([{url, URL}, {title, Title}, {links, Links}]),
            see_db_proxy:visited(DbNode, URL, {data, Title, Text}),
            lists:foreach(fun(Link) -> see_db_proxy:queue(DbNode, Link) end, Links);
        binary ->
            error_logger:info_msg("Got binary~n"),
            see_db_proxy:visited(DbNode, URL, binary);
        {redirect, Link} ->
            error_logger:info_msg("Got redirect: ~s~n", [Link]),
            RedirectURL = absolute_link(URL, Link),
            error_logger:info_report([{url, URL}, {redirect, RedirectURL}]),
            see_db_proxy:visited(DbNode, URL, {redirect, RedirectURL}),
            see_db_proxy:queue(DbNode, RedirectURL);
        {error, Reason} ->
            error_logger:info_msg("Got error~n"),
            error_logger:error_report([{url, URL}, {error, Reason}]),
            see_db_proxy:visited(DbNode, URL, {error, Reason})
    end.

absolute_link(URL, Link) ->
    ParsedURL = hackney_url:parse_url(URL),
    case re:run(Link, <<"[^/]+://">>) of
        nomatch ->
            DirName = filename:dirname(ParsedURL#hackney_url.path),
            hackney_url:unparse_url(ParsedURL#hackney_url{path = join_paths(DirName, Link)});
        _ ->
            Link
    end.

join_paths(Dir, Path) ->
    case filename:pathtype(Path) of
        absolute when is_list(Path) ->
            list_to_binary(Path);
        absolute when is_binary(Path) ->
            Path;
        relative ->
            filename:absname(Path, Dir)
    end.
