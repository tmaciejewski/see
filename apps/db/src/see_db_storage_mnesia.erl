-module(see_db_storage_mnesia).

-include_lib("see_db_records.hrl").

-export([create_tables/0,
         create_tables/1,
         start/0,
         stop/0,
         update_url/3,
         add_url/1,
         get_unvisited/0,
         set_unvisited/1,
         get_page/1,
         get_pages_from_index/1,
         get_words/1,
         get_page_count/0]).

create_tables() ->
    create_tables([node()]).

create_tables(Nodes) ->
    mnesia:create_table(see_pages,
                        [{record_name, page},
                         {index, [#page.last_visit]},
                         {attributes, record_info(fields, page)},
                         {disc_copies, Nodes}]),
    mnesia:create_table(see_index,
                        [{record_name, index},
                         {attributes, record_info(fields, index)},
                         {disc_copies, Nodes}]).

start() ->
    application:start(mnesia).

stop() ->
    application:stop(mnesia).

update_url(URL, Title, Words) ->
    transaction(fun() -> do_update_url(URL, Title, Words) end).

add_url(URL) ->
    transaction(fun() -> do_add_url(URL) end).

get_unvisited() ->
    transaction(fun() -> do_get_unvisited() end).

set_unvisited(URL) ->
    transaction(fun() -> do_set_unvisited(URL) end).

get_page(Id) ->
    transaction(fun() -> do_get_page(Id) end).

get_pages_from_index(Word) ->
    transaction(fun() -> do_get_pages_from_index(Word) end).

get_words(Id) ->
    transaction(fun() -> do_get_words(Id) end).

get_page_count() ->
    mnesia:table_info(see_pages, size).

%-------------------------------------------------------------------------

transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            throw({transaction_aborted, Reason})
    end.

do_update_url(URL, Title, Words) ->
    Id = erlang:phash2(URL),
    case remove_page(Id) of
        ok ->
            mnesia:write(see_pages, #page{id = Id, url = URL, title = Title, content = Words}, write),
            insert_words_to_index(Words, Id);
        not_found ->
            not_found
    end.

remove_page(Id) ->
    case mnesia:read(see_pages, Id) of
        [#page{content = Words}] when is_list(Words) ->
            lists:foreach(fun(Word) -> remove_from_index(Word, Id) end, Words);
        [#page{last_visit = pending}] ->
            ok;
        _Other ->
            not_found
    end.

remove_from_index(Word, Id) ->
    case mnesia:read(see_index, Word) of
        [] ->
            ok;
        [#index{pages = Pages}] ->
            mnesia:write(see_index, #index{word = Word, pages = sets:del_element(Id, Pages)}, write)
    end.

insert_words_to_index(Words, Id) when is_list(Words) ->
    lists:foreach(fun(Word) -> insert_word_to_index(Word, Id) end, Words);

insert_words_to_index(_, _) ->
    ok.

insert_word_to_index(Word, Id) ->
    case mnesia:read(see_index, Word) of
        [#index{pages = Pages}] ->
            mnesia:write(see_index, #index{word = Word, pages = sets:add_element(Id, Pages)}, write);
        [] ->
            mnesia:write(see_index, #index{word = Word, pages = sets:from_list([Id])}, write)
    end.

do_add_url(URL) ->
    Id = erlang:phash2(URL),
    case mnesia:read(see_pages, Id) of
        [] ->
            mnesia:write(see_pages, #page{id = Id, url = URL, last_visit = null}, write);
        _ ->
            ok
    end.

do_get_unvisited() ->
    case mnesia:index_read(see_pages, null, #page.last_visit) of
        [Page = #page{url = URL} | _] ->
            mnesia:write(see_pages, Page#page{last_visit = pending}, write),
            {ok, URL};
        [] ->
            nothing
    end.

do_set_unvisited(URL) ->
    Id = erlang:phash2(URL),
    case mnesia:read(see_pages, Id) of
        [Page = #page{last_visit = pending}] ->
            mnesia:write(see_pages, Page#page{last_visit = null}, write);
        _  ->
            ok
    end.

do_get_page(Id) ->
    [#page{title = Title, url = URL}] = mnesia:read(see_pages, Id),
    {URL, Title}.

do_get_pages_from_index(Word) ->
    case mnesia:read(see_index, Word) of
        [] ->
            sets:new();
        [#index{pages = Pages}] ->
            Pages
    end.

do_get_words(Id) ->
    case mnesia:read(see_pages, Id) of
        [#page{content = Words}] when is_list(Words) ->
            Words;
        _Other ->
            []
    end.
