-module(see_db_storage_ets_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, <<"url1">>).
-define(URL2, <<"url2">>).
-define(TITLE, <<"Page title">>).
-define(TITLE2, <<"Page title 2">>).
-define(WORD1, <<"word1">>).
-define(WORD2, <<"word2">>).

start() ->
    see_db_storage_ets:start().

stop(_) ->
    see_db_storage_ets:stop().

search(Word) ->
    Pages = sets:to_list(see_db_storage_ets:get_pages_from_index(Word)),
    lists:map(fun see_db_storage_ets:get_page/1, Pages).

when_no_url_added__gets_unvisited_returns_nothing__test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             ?_assertEqual(nothing, see_db_storage_ets:get_unvisited())
     end}.

when_new_url_is_added__gets_unvisited_returns_it_once__test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             see_db_storage_ets:add_url(?URL),
             [?_assertEqual({ok, ?URL}, see_db_storage_ets:get_unvisited()),
              ?_assertEqual(nothing, see_db_storage_ets:get_unvisited())]
     end}.

when_url_is_pending__set_unvisited_enqueues_it_back__test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             see_db_storage_ets:add_url(?URL),
             see_db_storage_ets:get_unvisited(),
             see_db_storage_ets:set_unvisited(?URL),
             ?_assertEqual({ok, ?URL}, see_db_storage_ets:get_unvisited())
     end}.

when_url_is_pending__update_url_sets_add_to_index_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             see_db_storage_ets:add_url(?URL),
             {ok, ?URL} = see_db_storage_ets:get_unvisited(),
             see_db_storage_ets:update_url(?URL, ?TITLE, [?WORD1, ?WORD2]),
             [?_assertEqual([{?URL, ?TITLE}], search(?WORD1)),
              ?_assertEqual([{?URL, ?TITLE}], search(?WORD2))]
     end}.

when_url_is_revisited__update_url_updates_index_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             see_db_storage_ets:add_url(?URL),
             {ok, ?URL} = see_db_storage_ets:get_unvisited(),
             see_db_storage_ets:update_url(?URL, ?TITLE, [?WORD1]),
             see_db_storage_ets:update_url(?URL, ?TITLE, [?WORD2]),
             [?_assertEqual([], search(?WORD1)),
              ?_assertEqual([{?URL, ?TITLE}], search(?WORD2))]
     end}.

when_two_urls_has_the_same_word__update_url_adds_them_both_to_index_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             see_db_storage_ets:add_url(?URL),
             see_db_storage_ets:add_url(?URL2),
             see_db_storage_ets:get_unvisited(),
             see_db_storage_ets:get_unvisited(),
             see_db_storage_ets:update_url(?URL, ?TITLE, [?WORD1]),
             see_db_storage_ets:update_url(?URL2, ?TITLE2, [?WORD1]),
             [?_assertEqual([{?URL, ?TITLE}, {?URL2, ?TITLE2}], search(?WORD1))]
     end}.

when_url_is_already_indexed__add_url_does_nothing_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             see_db_storage_ets:add_url(?URL),
             see_db_storage_ets:get_unvisited(),
             see_db_storage_ets:update_url(?URL, ?TITLE, [?WORD1]),
             see_db_storage_ets:add_url(?URL),
             [?_assertEqual([{?URL, ?TITLE}], search(?WORD1))]
     end}.

when_no_url_has_given_word__return_empty_set_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             [?_assertEqual([], search(?WORD1))]
     end}.
