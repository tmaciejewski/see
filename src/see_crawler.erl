-module(see_crawler).
-behaviour(gen_server).

-export([start_link/0,
         stop/1,
         visit/1]).

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

visit("http://" ++ URL) ->
    visit(URL);

visit(URL) ->
    case httpc:request("http://" ++ URL) of
        {ok, {{_, Code, _}, Headers, Content}} ->
            MIME = hd(string:tokens(proplists:get_value("content-type", Headers), ";")),
            case is_text(MIME) of
                true ->
                    Links = links(URL, Content),
                    see_db:visit(URL, Code, stripPage(Content)),
                    see_db:links(Links, URL),
                    lists:foreach(fun(Link) -> see_db:queue(Link) end, Links);
                false ->
                    see_db:visit(URL, binary, MIME)
            end;

        {error, Reason} ->
            error_logger:error_report([{url, URL}, {error, Reason}]),
            see_db:visit(URL, error, Reason)
    end.

%----------------------------------------------------------

init(Args) ->
    {ok, Args, ?SLEEP_TIMEOUT}.

handle_info(timeout, State) ->
    case see_db:next() of
        nothing ->
            {noreply, State, ?SLEEP_TIMEOUT};
        {ok, Next} ->
            see_db:visit(Next, undefined, visiting),
            visit(Next),
            {noreply, State, 1}
   end.

is_text("text/html") -> true;
is_text("text/plain") -> true;
is_text(_) -> false.

absLink(_, "http://" ++ Link) ->
    Link;

absLink(URL, "/" ++ Link) ->
    [Host|_] = string:tokens(URL, "/"),
    string:join([Host, Link], "/");

absLink(_, _) ->
    "".

links(URL, WebPage) ->
    case  re:run(WebPage, "<a *href=\"([^\"# ]*)", 
                [global, {capture, [1], list}]) of
        {match, Match} ->
            Links = lists:map(fun(Link) -> absLink(URL, Link) end, 
                lists:append(Match)),
            lists:filter(fun(X) -> length(X) > 0 end, Links);
        nomatch -> []
    end.  

stripPage(Content) ->
    Body = re:replace(Content, ".*<body>(.*)</body>.*", "\1", 
        [global, {return, list}, caseless]),
    re:replace(Body, "<[^>]+>", "", [global, {return, list}]).

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_, _) ->
    ok.
    
code_change(_OldVsn, State, _) ->
    {ok, State}.
