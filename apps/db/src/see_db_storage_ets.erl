-module(see_db_storage_ets).

-record(page, {id, url, title, content, last_visit = erlang:timestamp()}).
-record(index, {word, pages}).

-export([init/0,
         update_url/2,
         update_url/3,
         add_url/1,
         get_unvisited/0,
         set_unvisited/1,
         get_page/1,
         get_pages_from_index/1]).

init() ->
    ets:new(see_pages, [named_table, {keypos, #page.id}]),
    ets:new(see_index, [named_table, {keypos, #index.word}]).

update_url(URL, Content) ->
    Id = erlang:phash2(URL),
    case remove_page(Id) of
        ok ->
            ets:insert(see_pages, #page{id = Id, url = URL, title = [], content = Content});
        not_found ->
            not_found
    end.

update_url(URL, Title, Words) ->
    Id = erlang:phash2(URL),
    case remove_page(Id) of
        ok ->
            ets:insert(see_pages, #page{id = Id, url = URL, title = Title, content = Words}),
            lists:foreach(fun(Word) -> insert_to_index(Word, Id) end, Words);
        not_found ->
            not_found
    end.

remove_page(Id) ->
    case ets:lookup(see_pages, Id) of
        [#page{last_visit = pending, content = Words}] when is_list(Words) ->
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

insert_to_index(Word, Id) ->
    case ets:lookup(see_index, Word) of
        [#index{pages = Pages}] ->
            ets:insert(see_index, #index{word = Word, pages = sets:add_element(Id, Pages)});
        [] ->
            ets:insert(see_index, #index{word = Word, pages = sets:from_list([Id])})
    end.

add_url(URL) ->
    Id = erlang:phash2(URL),
    case ets:lookup(see_pages, Id) of
        [] ->
            ets:insert(see_pages, #page{id = Id, url = URL, last_visit = null});
        _ ->
            ok
    end.

get_unvisited() ->
    case ets:match_object(see_pages, #page{last_visit = null, _ = '_'}, 1) of
        {[Page = #page{url = URL}], _} ->
            ets:insert(see_pages, Page#page{last_visit = pending}),
            {ok, URL};
        '$end_of_table' ->
            nothing
    end.

set_unvisited(URL) ->
    Id = erlang:phash2(URL),
    case ets:lookup(see_pages, Id) of
        [Page = #page{last_visit = pending}] ->
            ets:insert(see_pages, Page#page{last_visit = null});
        _  ->
            ok
    end.

get_page(Id) ->
    [#page{title = Title, url = URL}] = ets:lookup(see_pages, Id),
    {URL, Title}.

get_pages_from_index(Word) ->
    case ets:lookup(see_index, Word) of
        [] ->
            sets:new();
        [#index{pages = Pages}] ->
            Pages
    end.
