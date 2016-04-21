-module(see_db_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, "url").
-define(CODE, 200).
-define(CONTENT, "aaa ddd eee fff").

-define(URL2, "url2").
-define(CODE2, 200).
-define(CONTENT2, "bbb ddd eee ggg").

-define(URL3, "url3").
-define(CODE3, 200).
-define(CONTENT3, "ccc ddd fff ggg").

-define(assert_search_result(URLs, Phrase),
        ?_assertEqual(lists:sort(URLs), lists:sort(see_db:search(Phrase)))).

start() ->
    {ok, Pid} = see_db:start(),
    ?assert(is_pid(Pid)),
    Pid.

stop(_) ->
    see_db:stop().

queued_page() ->
    Pid = start(),
    see_db:queue(?URL),
    Pid.

visited_page() ->
    Pid = start(),
    see_db:visited(?URL, ?CODE, ?CONTENT),
    Pid.

visited_many_pages() ->
    Pid = start(),
    see_db:visited(?URL, ?CODE, ?CONTENT),
    see_db:visited(?URL2, ?CODE2, ?CONTENT2),
    see_db:visited(?URL3, ?CODE3, ?CONTENT3),
    Pid.

visited_many_same_pages() ->
    Pid = start(),
    see_db:visited(?URL, ?CODE, ?CONTENT),
    see_db:visited(?URL2, ?CODE, ?CONTENT),
    see_db:visited(?URL3, ?CODE, ?CONTENT),
    Pid.

visited_page_has_changed() ->
    Pid = start(),
    see_db:visited(?URL, ?CODE, ?CONTENT),
    see_db:visited(?URL, ?CODE, ?CONTENT2),
    Pid.

when_no_queued_pages__next_returns_nothing_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             ?_assertEqual(nothing, see_db:next())
     end}.

when_queued_page__next_returns_its_url_test_() ->
    {setup, fun queued_page/0, fun stop/1,
     fun(_) ->
             ?_assertEqual(?URL, see_db:next())
     end}.

when_all_pages_visited__next_returns_nothing_test_() ->
    {setup, fun visited_page/0, fun stop/1,
     fun(_) ->
             ?_assertEqual(nothing, see_db:next())
     end}.

when_word_is_not_present__search_returns_empty_list_test_() ->
    {setup, fun visited_page/0, fun stop/1,
     fun(_) ->
             ?assert_search_result([], "dfsd")
     end}.

when_word_is_present_on_one_page__search_returns_single_page_list_test_() ->
    {setup, fun visited_page/0, fun stop/1,
     fun(_) ->
             [?assert_search_result([?URL], Word) || Word <- string:tokens(?CONTENT, " ")]
     end}.

when_phrase_is_present_on_one_page__search_returns_single_page_list_test_() ->
    {setup, fun visited_many_pages/0, fun stop/1,
     fun(_) ->
             [?assert_search_result([?URL], ?CONTENT),
              ?assert_search_result([?URL2], ?CONTENT2),
              ?assert_search_result([?URL3], ?CONTENT3)]
     end}.

when_word_is_present_on_many_pages__search_returns_them_all_test_() ->
    {setup, fun visited_many_pages/0, fun stop/1,
     fun(_) ->
             [?assert_search_result([?URL], "aaa"),
              ?assert_search_result([?URL2], "bbb"),
              ?assert_search_result([?URL3], "ccc"),
              ?assert_search_result([?URL, ?URL2], "eee"),
              ?assert_search_result([?URL, ?URL3], "fff"),
              ?assert_search_result([?URL2, ?URL3], "ggg"),
              ?assert_search_result([?URL, ?URL2, ?URL3], "ddd")]
     end}.

when_many_words_are_given__search_returns_pages_containing_all_of_them_test_() ->
    {setup, fun visited_many_same_pages/0, fun stop/1,
     fun(_) ->
              [?assert_search_result([?URL, ?URL2, ?URL3], "aaa ddd"),
               ?assert_search_result([?URL, ?URL2, ?URL3], "aaa ddd eee"),
               ?assert_search_result([?URL, ?URL2, ?URL3], "aaa ddd eee fff")]
     end}.

when_page_changes__search_returns_only_new_content_test_() ->
    {setup, fun visited_page_has_changed/0, fun stop/1,
     fun(_) ->
              [?assert_search_result([], "aaa"),
               ?assert_search_result([?URL], "ddd"),
               ?assert_search_result([?URL], "ggg")]
     end}.
