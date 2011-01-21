-module(see_db).
-behaviour(gen_server).

-export([start/0,
         start_link/0,
         stop/0,
         visit/3,
         link/2,
         links/2,
         queue/1,
         next/0,
         search/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("stdlib/include/qlc.hrl"). 

-record(page, {url, code, content, last_visit}).
-record(link, {to, from}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

visit(URL, Code, Content) ->
    gen_server:cast(?MODULE, {visit, URL, Code, Content}).

link(To, From) ->
    gen_server:cast(?MODULE, {link, To, From}).

links(Links, From) ->
    lists:foreach(fun(Link) -> link(Link, From) end, Links).

queue(URL) ->
    gen_server:cast(?MODULE, {queue, URL}).

next() ->
    gen_server:call(?MODULE, next).

search(Phrases) ->
    gen_server:call(?MODULE, {search, Phrases}).

%----------------------------------------------------------

init(_Args) ->
    mnesia:create_table(page, [{disc_only_copies, [node()]}, 
             {attributes, record_info(fields, page)}]),
    mnesia:create_table(link, [{disc_only_copies, [node()]}, 
             {attributes, record_info(fields, link)},
             {type, bag}]),

    {ok, []}.

terminate(_, _) ->
    ok.

handle_cast({visit, URL, Code, Content}, State) ->
    F = fun() -> 
            mnesia:write(#page{url = URL, code = Code,
                    content = Content, last_visit = now()})
    end,
    mnesia:transaction(F),
    {noreply, State};

handle_cast({link, To, From}, State) ->
    F = fun() -> mnesia:write(#link{to = To, from = From}) end,
    mnesia:transaction(F),
    {noreply, State};

handle_cast({queue, URL}, State) ->
    case mnesia:transaction(fun() -> mnesia:read({page, URL}) end) of
        {atomic, []} ->
            mnesia:transaction(fun() -> mnesia:write(#page{url = URL,
                                last_visit = now()}) end);
        {atomic, _}  ->
            ok 
    end,
    {noreply, State}.

handle_call(stop, _, State) ->
    {stop, shutdown, ok, State};

handle_call(next, _, State) ->
    F = fun() -> 
        Q = qlc:q([{Page#page.url, Page#page.last_visit} || 
                    Page <- mnesia:table(page), Page#page.content == undefined]),
        qlc:eval(qlc:sort(Q, {order, fun({_, A}, {_, B}) -> 
                            timer:now_diff(A, B) < 0 end}))
    end,
    case mnesia:transaction(F) of
       {atomic, [{Next, _}|_]} -> 
           {reply, {ok, Next}, State};
       {atomic, []} ->
           {reply, nothing, State}
    end;

handle_call({search, Phrases}, _, State) ->
    F = fun() -> 
            Q = qlc:q([{Page#page.url, lists:sum(occurs(Phrases, Page#page.content))} || Page <- mnesia:table(page), 
                Page#page.code == 200, lists:all(fun(X) -> X > 0 end, occurs(Phrases, Page#page.content))]),
        qlc:eval(qlc:sort(Q, {order, fun({_, A}, {_, B}) -> A > B end}))
    end,
    case mnesia:transaction(F) of
        {atomic, Results} ->
            {reply, {ok, Results}, State};
        {aborted, Reason} ->
            {reply, {error, Reason}, State}
    end.

handle_info(_, State) ->
    {noreply, State}.

occurs(Phrases, Content) ->
    F = fun(Phrase) ->
        case re:run(Content, Phrase, [global, caseless]) of
            nomatch ->
                0;
            {match, Match} ->
                length(Match)
        end
    end, 
    lists:map(F, Phrases).
    
code_change(_OldVsn, State, _) ->
    {ok, State}.
