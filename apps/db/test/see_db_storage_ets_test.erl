-module(see_db_storage_ets_test).

-include_lib("eunit/include/eunit.hrl").
-include("see_db_storage_test.hrl").

start() ->
    ok = see_db_storage_ets:start(),
    see_db_storage_ets.

stop(see_db_storage_ets) ->
    see_db_storage_ets:stop().

see_db_storage_ets_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun when_no_url_added__gets_unvisited_returns_nothing__test_/1,
      fun when_new_url_is_added__gets_unvisited_returns_it_once__test_/1,
      fun when_url_is_pending__set_unvisited_enqueues_it_back__test_/1,
      fun when_url_is_pending__update_url_sets_add_to_index_test_/1,
      fun when_url_is_visited__get_words_return_page_words_test_/1,
      fun when_url_is_revisited__update_url_updates_index_test_/1,
      fun when_two_urls_has_the_same_word__update_url_adds_them_both_to_index_test_/1,
      fun when_url_is_already_indexed__add_url_does_nothing_test_/1,
      fun when_no_url_has_given_word__return_empty_set_test_/1
     ]}.
