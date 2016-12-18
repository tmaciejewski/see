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
    case rpc:call(DbNode, see_db_srv, next, []) of
        nothing ->
            error_logger:info_msg("Nothing to do"),
            {noreply, State, ?IDLE_TIMEOUT};
        {ok, Next} ->
            error_logger:info_msg("Visiting " ++ Next),
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
