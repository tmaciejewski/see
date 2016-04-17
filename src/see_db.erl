-module(see_db).
-behaviour(gen_server).

-export([start/0,
         start_link/0,
         stop/0,
         visited/3,
         queue/1,
         next/0,
         search/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(page, {id, url, code, last_visit = erlang:timestamp()}).
-record(index, {word, pages}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

visited(URL, Code, Content) ->
    gen_server:cast(?MODULE, {visited, URL, Code, Content}).

queue(URL) ->
    gen_server:cast(?MODULE, {queue, URL}).

next() ->
    gen_server:call(?MODULE, next).

search(Phrases) ->
    gen_server:call(?MODULE, {search, Phrases}).

%----------------------------------------------------------

init(_Args) ->
    PagesTid = ets:new(pages, [{keypos, #page.id}]),
    IndexTid = ets:new(index, [{keypos, #index.word}]),
    {ok, {PagesTid, IndexTid}}.

terminate(_, _) ->
    ok.

handle_cast({visited, URL, Code, Content}, {PagesTid, IndexTid}) ->
    Id = erlang:phash2(URL),
    ets:insert(PagesTid, #page{id = Id, url = URL, code = Code}),
    Words = string:tokens(Content, " "),
    lists:foreach(fun(Word) -> update_word(IndexTid, Word, Id) end, Words),
    {noreply, {PagesTid, IndexTid}};

handle_cast({queue, URL}, {PagesTid, _} = State) ->
    Id = erlang:phash2(URL),
    ets:insert(PagesTid, #page{id = Id, url = URL, code = null, last_visit = null}),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_call(stop, _, State) ->
    {stop, shutdown, ok, State};

handle_call(next, _, {PagesTid, _} = State) ->
    case ets:match(PagesTid, #page{last_visit = null, url = '$1', _ = '_'}, 1) of
        {[[URL]], _} ->
            {reply, URL, State};
        _ ->
            {reply, nothing, State}
    end;

handle_call({search, Word}, _, {PagesTid, IndexTid} = State) ->
    case ets:lookup(IndexTid, Word) of
        [] ->
            {reply, [], State};
        [#index{pages = Pages}] ->
            {reply, lists:map(fun(PageId) ->
                              [#page{url = URL}] = ets:lookup(PagesTid, PageId),
                              URL
                      end, Pages), State}
    end;

handle_call(_, _, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _) ->
    {ok, State}.

%----------------------------------------------------------

update_word(IndexTid, Word, Id) ->
    case ets:lookup(IndexTid, Word) of
        [#index{pages = Pages}] ->
            ets:insert(IndexTid, #index{word = Word, pages = [Id|Pages]});
        [] ->
            ets:insert(IndexTid, #index{word = Word, pages = [Id]})
    end.
