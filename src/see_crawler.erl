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
            {noreply, State, ?SLEEP_TIMEOUT};
        {ok, Next} ->
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

visit("http://" ++ URL) ->
    visit(URL);

visit(URL) ->
    case httpc:request("http://" ++ URL) of
        {ok, {{_, _Code, _}, Headers, Content}} ->
            case see_html:is_text(Headers) of
                true ->
                    Words = see_html:words(Content),
                    see_db:visited(URL, Words);
                false ->
                    see_db:visited(URL, binary)
            end;

        {error, Reason} ->
            error_logger:error_report([{url, URL}, {error, Reason}]),
            see_db:visited(URL, {error, Reason})
    end.

