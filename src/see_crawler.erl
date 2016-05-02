-module(see_crawler).
-behaviour(gen_server).

-export([start_link/0,
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

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%----------------------------------------------------------

init(Args) ->
    {ok, Args, ?SLEEP_TIMEOUT}.

handle_info(timeout, State) ->
    case see_db:next() of
        nothing ->
            error_logger:info_msg("Nothing to do"),
            {noreply, State, ?SLEEP_TIMEOUT};
        {ok, Next} ->
            error_logger:info_msg("Visiting " ++ Next),
            visit(Next),
            {noreply, State, ?SLEEP_TIMEOUT}
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

visit(URL) ->
    case get_url(URL) of
        {ok, Content} ->
            Words = see_html:words(Content),
            Links = see_html:links(URL, Content),
            see_db:visited(URL, Words),
            lists:foreach(fun see_db:queue/1, Links);
        binary ->
            see_db:visited(URL, binary);
        {redirect, RedirectURL} ->
            see_db:visited(URL, {redirect, RedirectURL}),
            see_db:queue(RedirectURL);
        {error, Reason} ->
            error_logger:error_report([{url, URL}, {error, Reason}]),
            see_db:visited(URL, {error, Reason})
    end.
