-module(see_db_storage_ets).
-behaviour(gen_server).

-include_lib("see_db_records.hrl").

-export([start/0,
         stop/0,
         update_url/3,
         add_url/1,
         get_unvisited/0,
         set_unvisited/1,
         get_page/1,
         get_pages_from_index/1,
         get_words/1,
         get_page_count/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start() ->
    case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

stop() ->
    gen_server:call(?MODULE, stop).

update_url(URL, Title, Words) ->
    gen_server:call(?MODULE, {update_url, URL, Title, Words}).

add_url(URL) ->
    gen_server:call(?MODULE, {add_url, URL}).

get_unvisited() ->
    gen_server:call(?MODULE, get_unvisited).

set_unvisited(URL) ->
    gen_server:call(?MODULE, {set_unvisited, URL}).

get_page(Id) ->
    gen_server:call(?MODULE, {get_page, Id}).

get_pages_from_index(Word) ->
    gen_server:call(?MODULE, {get_pages_from_index, Word}).

get_words(Id) ->
    gen_server:call(?MODULE, {get_words, Id}).

get_page_count() ->
    gen_server:call(?MODULE, get_page_count).

%%-------------------------------------------------------------

init(_) ->
    ets:new(see_pages, [named_table, {keypos, #page.id}]),
    ets:new(see_index, [named_table, {keypos, #index.word}]),
    {ok, []}.

terminate(_, _) ->
    ets:delete(see_index),
    ets:delete(see_pages).

handle_call(stop, _, State) ->
    {stop, shutdown, ok, State};

handle_call({update_url, URL, Title, Words}, _, State) ->
    Id = erlang:phash2(URL),
    case remove_page(Id) of
        ok ->
            ets:insert(see_pages, #page{id = Id, url = URL, title = Title, content = Words}),
            insert_words_to_index(Words, Id),
            {reply, ok, State};
        not_found ->
            {reply, not_found, State}
    end;

handle_call({add_url, URL}, _, State) ->
    Id = erlang:phash2(URL),
    case ets:lookup(see_pages, Id) of
        [] ->
            ets:insert(see_pages, #page{id = Id, url = URL, last_visit = null}),
            {reply, ok, State};
        _ ->
            {reply, ok, State}
    end;

handle_call(get_unvisited, _, State) ->
    case ets:match_object(see_pages, #page{last_visit = null, _ = '_'}, 1) of
        {[Page = #page{url = URL}], _} ->
            ets:insert(see_pages, Page#page{last_visit = pending}),
            {reply, {ok, URL}, State};
        '$end_of_table' ->
            {reply, nothing, State}
    end;

handle_call({set_unvisited, URL}, _, State) ->
    Id = erlang:phash2(URL),
    case ets:lookup(see_pages, Id) of
        [Page = #page{last_visit = pending}] ->
            ets:insert(see_pages, Page#page{last_visit = null}),
            {reply, ok, State};
        _  ->
            {reply, ok, State}
    end;

handle_call({get_page, Id}, _, State) ->
    [#page{title = Title, url = URL}] = ets:lookup(see_pages, Id),
    {reply, {URL, Title}, State};

handle_call({get_pages_from_index, Word}, _, State) ->
    case ets:lookup(see_index, Word) of
        [] ->
            {reply, sets:new(), State};
        [#index{pages = Pages}] ->
            {reply, Pages, State}
    end;

handle_call({get_words, Id}, _, State) ->
    case ets:lookup(see_pages, Id) of
        [#page{content = Words}] when is_list(Words) ->
            {reply, Words, State};
        _Other ->
            {reply, [], State}
    end;

handle_call(get_page_count, _, State) ->
    {reply, ets:info(see_pages, size), State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _) ->
    {ok, State}.

remove_page(Id) ->
    case ets:lookup(see_pages, Id) of
        [#page{content = Words}] when is_list(Words) ->
            lists:foreach(fun(Word) -> remove_from_index(Word, Id) end, Words);
        [#page{last_visit = pending}] ->
            ok;
        _Other ->
            not_found
    end.

remove_from_index(Word, Id) ->
    case ets:lookup(see_index, Word) of
        [] ->
            ok;
        [#index{pages = Pages}] ->
            ets:insert(see_index, #index{word = Word, pages = sets:del_element(Id, Pages)})
    end.

insert_words_to_index(Words, Id) when is_list(Words) ->
    lists:foreach(fun(Word) -> insert_word_to_index(Word, Id) end, Words);

insert_words_to_index(_, _) ->
    ok.

insert_word_to_index(Word, Id) ->
    case ets:lookup(see_index, Word) of
        [#index{pages = Pages}] ->
            ets:insert(see_index, #index{word = Word, pages = sets:add_element(Id, Pages)});
        [] ->
            ets:insert(see_index, #index{word = Word, pages = sets:from_list([Id])})
    end.
