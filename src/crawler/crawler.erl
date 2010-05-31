-module(crawler).
-compile(export_all).

-import(http).

start() ->
    gen_server:start(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

visit(Pid, URL) ->
    gen_server:cast(Pid, {visit, URL}).

init(Args) ->
    {ok, Args, 3000}.

links(WebPage) ->
    case  re:run(WebPage, "<a *href=\"([^\"]*)\"", 
                [global, {capture, [1], list}]) of
        {match, Match} ->
            lists:append(Match);
        nomatch -> []
    end.  

handle_info(timeout, State) ->
    io:format("Timeout!\n"),
    {noreply, State, 1000}.

handle_cast({visit, URL}, State) ->
    {Code, Content} = http:getPage(URL),
    io:format("Status: ~w\nLinks: ~p\n", [Code, links(Content)]),
    %visit(self(), URL),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(normal, _) ->
    io:format("Stopping...\n").

    
