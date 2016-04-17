-module(see_db_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, "url").
-define(CODE, 200).
-define(CONTENT, "aaa bbb ccc").

-define(URL2, "url2").
-define(CODE2, 200).
-define(CONTENT2, "bbb ddd").

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
             ?_assertEqual([], see_db:search("dfsd"))
     end}.

when_word_is_present_on_one_page__search_returns_single_page_list_test_() ->
    {setup, fun visited_page/0, fun stop/1,
     fun(_) ->
             [?_assertEqual([?URL], see_db:search("aaa")),
              ?_assertEqual([?URL], see_db:search("bbb")),
              ?_assertEqual([?URL], see_db:search("ccc"))]
     end}.

when_word_is_present_on_many_pages__search_returns_them_all_test_() ->
    {setup, fun visited_many_pages/0, fun stop/1,
     fun(_) ->
             [?_assertEqual([?URL], see_db:search("aaa")),
              ?_assertEqual([?URL2, ?URL], see_db:search("bbb")),
              ?_assertEqual([?URL], see_db:search("ccc")),
              ?_assertEqual([?URL2], see_db:search("ddd"))]
     end}.
