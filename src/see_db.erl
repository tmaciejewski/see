-module(see_db).
-behaviour(gen_server).

-export([start/0,
         start_link/0,
         stop/0,
         visited/2,
         queue/1,
         next/0,
         search/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(page, {id, url, words, last_visit = erlang:timestamp()}).
-record(index, {word, pages}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

visited(URL, Words) ->
    gen_server:cast(?MODULE, {visited, URL, Words}).

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

handle_cast({visited, URL, {error, Reason}}, {PagesTid, IndexTid}) ->
    Id = erlang:phash2(URL),
    remove_from_index(IndexTid, PagesTid, Id),
    ets:insert(PagesTid, #page{id = Id, url = URL, words = {error, Reason}}),
    {noreply, {PagesTid, IndexTid}};

handle_cast({visited, URL, binary}, {PagesTid, IndexTid}) ->
    Id = erlang:phash2(URL),
    remove_from_index(IndexTid, PagesTid, Id),
    ets:insert(PagesTid, #page{id = Id, url = URL, words = binary}),
    {noreply, {PagesTid, IndexTid}};

handle_cast({visited, URL, Words}, {PagesTid, IndexTid}) ->
    Id = erlang:phash2(URL),
    remove_from_index(IndexTid, PagesTid, Id),
    ets:insert(PagesTid, #page{id = Id, url = URL, words = Words}),
    insert_to_index(IndexTid, Words, Id),
    {noreply, {PagesTid, IndexTid}};

handle_cast({queue, URL}, {PagesTid, _} = State) ->
    Id = erlang:phash2(URL),
    ets:insert(PagesTid, #page{id = Id, url = URL, last_visit = null}),
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

handle_call({search, Phrases}, _, {PagesTid, IndexTid} = State) ->
    Words = string:tokens(Phrases, " "),
    PageLists = [get_pages(Word, IndexTid) || Word <- Words],
    Result = merge_page_lists(PageLists),
    {reply, [get_url(Id, PagesTid) || Id <- Result], State};

handle_call(_, _, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _) ->
    {ok, State}.

%----------------------------------------------------------

remove_from_index(IndexTid, PagesTid, Id) ->
    case ets:lookup(PagesTid, Id) of
        [#page{words = Words}] when is_list(Words) ->
            lists:foreach(fun(Word) -> remove_from_word(IndexTid, Word, Id) end, Words),
            ets:delete(PagesTid, Id);
        [] ->
            ok
    end.

remove_from_word(IndexTid, Word, Id) ->
    case ets:lookup(IndexTid, Word) of
        [] ->
            ok;
        [#index{pages = Pages}] ->
            ets:insert(IndexTid, #index{word = Word, pages = lists:delete(Id, Pages)})
    end.

insert_to_index(_, [], _) ->
    ok;

insert_to_index(IndexTid, [Word | Words], Id) ->
    case ets:lookup(IndexTid, Word) of
        [#index{pages = Pages}] ->
            ets:insert(IndexTid, #index{word = Word, pages = lists:sort([Id|Pages])});
        [] ->
            ets:insert(IndexTid, #index{word = Word, pages = [Id]})
    end,
    insert_to_index(IndexTid, Words, Id).

get_url(Id, PagesTid) ->
    [#page{url = URL}] = ets:lookup(PagesTid, Id),
    URL.

get_pages(Word, IndexTid) ->
    case ets:lookup(IndexTid, Word) of
        [] ->
            [];
        [#index{pages = Pages}] ->
            Pages
    end.

merge_page_lists([]) ->
    [];

merge_page_lists([List]) ->
    List;

merge_page_lists([List1, List2 | Rest]) ->
    merge_page_lists([merge_two_page_lists(List1, List2) | Rest]).

merge_two_page_lists(List1, List2) ->
    lists:reverse(merge_two_page_lists(List1, List2, [])).

merge_two_page_lists([], _, Res) ->
    Res;

merge_two_page_lists(_, [], Res) ->
    Res;

merge_two_page_lists([Page1 | List1], [Page2 | List2], Res) when Page1 < Page2 ->
    merge_two_page_lists(List1, [Page2 | List2], Res);

merge_two_page_lists([Page1 | List1], [Page2 | List2], Res) when Page1 > Page2 ->
    merge_two_page_lists([Page1 | List1], List2, Res);

merge_two_page_lists([Page | List1], [_ | List2], Res) ->
    merge_two_page_lists(List1, List2, [Page | Res]).
