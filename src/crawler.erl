-module(crawler).
-compile(export_all).

-import(http).
-import(db).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

visit(Pid, URL) ->
    gen_server:cast(Pid, {visit, URL}).

init(Args) ->
    {ok, Args, 0}.

links(_URL, WebPage) ->
    case  re:run(WebPage, "<a *href=\"(http://[^\"# ]*)", 
                [global, {capture, [1], list}]) of
        {match, Match} ->
            lists:append(Match);
        nomatch -> []
    end.  

handle_info(timeout, State) ->
    case db:next() of
        nothing ->
            {noreply, State, 1000};
        {ok, Next} ->
            handle_cast({visit, Next}, State),
            {noreply, State, 1}
   end.

is_text("text/html") -> true;
is_text("text/plain") -> true;
is_text(_) -> false.

handle_cast({visit, URL}, State) ->
    io:format("Getting ~s... ", [URL]),
    case http:getPage(URL) of
        {ok, Code, Headers, Content} ->
            io:format(" ~w\n", [Code]),
            {ok, Type} = http:header("Content-Type", Headers),
            case is_text(hd(string:tokens(Type, ";"))) of
                true ->
                    Links = links(URL, Content),
                    db:visit(URL, Code, Content),
                    db:links(Links, URL),
                    db:queue(Links);
                false ->
                    db:visit(URL, Code, "<binary>")
            end;

        {error, Reason} ->
            db:visit(URL, error, Reason)
    end,
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(normal, _) ->
    io:format("Stopping...\n").

    
