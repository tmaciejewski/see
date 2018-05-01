-module(see_db_srv_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, <<"http://www.foo.com/">>).
-define(TITLE, "title").

-define(URL2, <<"http://url2/">>).
-define(TITLE2, "title 2").

-define(URL3, <<"http://url3/">>).
-define(TITLE3, "title 3").

-define(assert_search_result(Results, Phrase),
        ?_assertEqual(lists:sort(Results), lists:sort(see_db_srv:search(Phrase)))).

start(Options) ->
    meck:new(see_text),
    meck:expect(see_text, extract_words, fun(X) -> binary:split(X, <<" ">>, [global, trim_all]) end),
    meck:new(storage_mock, [non_strict]),
    meck:expect(storage_mock, start, fun() -> ok end),
    meck:expect(storage_mock, stop, fun() -> ok end),
    meck:new(rank_mock, [non_strict]),
    meck:expect(rank_mock, rank, fun(_, _, _) -> 1.0 end),
    {ok, Pid} = see_db_srv:start([{rank, rank_mock}, {storage, storage_mock} | Options]),
    ?assert(is_pid(Pid)),
    Pid.

start() ->
    start([]).

start_with_domain_filter() ->
    start([{domain_filter, "foo"}]).

stop(_) ->
    see_db_srv:stop(),
    ?assert(meck:validate(see_text)),
    ?assert(meck:validate(storage_mock)),
    ?assert(meck:validate(rank_mock)),
    meck:unload(see_text),
    meck:unload(storage_mock),
    meck:unload(rank_mock).

find_timer(Pid) ->
    ets:match(timer_tab, {'_', timeout, {timer, send, [Pid, '$1']}}).

trigger_timeout(Pid) ->
    [[Msg]] = find_timer(Pid),
    Pid ! Msg.

when_storage_start_fails__init_fails_test_() ->
    {setup,
     fun() -> 
             meck:new(storage_mock, [non_strict]),
             meck:expect(storage_mock, start, fun() -> {error, fail} end)
     end,
     fun(_) ->
             ?assert(meck:validate(storage_mock)),
             meck:unload(storage_mock)
     end,
     fun(_) ->
             ?_assertEqual({error, fail}, see_db_srv:start([{storage, storage_mock}]))
     end}.

when_no_queued_urls__next_returns_nothing_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, get_unvisited, fun() -> nothing end),
             ?_assertEqual(nothing, see_db_srv:next())
     end}.

when_queued_url__next_returns_it_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, get_unvisited, fun() -> {ok, ?URL} end),
             ?_assertEqual({ok, ?URL}, see_db_srv:next())
     end}.

when_url_has_no_schema__set_http_as_default_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, add_url, fun(<<"http://www.url.com/">>) -> ok end),
             ?_assertEqual(ok, see_db_srv:queue(<<"www.url.com/">>))
     end}.

when_url_is_invalid__queue_returns_error_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             [%?_assertEqual(url_error, see_db_srv:queue(<<"ftp://www.wrong.url">>)),
              % disabled because: https://github.com/benoitc/hackney/issues/468
              ?_assertEqual(url_error, see_db_srv:queue(<<"www:wrong:url">>))]
     end}.

when_queued_url_with_no_path__root_path_is_added_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, add_url, fun(<<"http://www.url.com/">>) -> ok end),
             ?_assertEqual(ok, see_db_srv:queue(<<"http://www.url.com">>))
     end}.

when_queued_url_with_fragment__fragment_is_discared_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, add_url, fun(<<"http://www.url.com/foo?query">>) -> ok end),
             ?_assertEqual(ok, see_db_srv:queue(<<"http://www.url.com/foo?query#fragment">>))
     end}.

when_queued_url_with_nonsimple_path__it_is_simplified_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, add_url, fun(<<"http://www.url.com/foo/baz/">>) -> ok end),
             ?_assertEqual(ok, see_db_srv:queue(<<"http://www.url.com/foo/bar/bar/../../bar/../baz/">>))
     end}.

when_domain_filter_is_given__queueing_only_accepts_matching_urls_test_() ->
    {setup, fun start_with_domain_filter/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, add_url, [{[<<"http://www.foo.com/">>], ok},
                                                 {[ <<"http://www.foo.bar.com/">>], ok}]),

             [?_assertEqual(ok, see_db_srv:queue(<<"http://www.foo.com/">>)),
              ?_assertEqual(ok, see_db_srv:queue(<<"http://www.foo.bar.com/">>)),
              ?_assertEqual(filter_mismatch, see_db_srv:queue(<<"http://www.bar.com/foo">>))]
     end}.

when_encoded_url_is_queued__it_is_returned_decoded_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, add_url, [{[<<"https://pl.wikipedia.org/wiki/Wikipedia:Strona_g%c5%82%c3%b3wna">>], ok}]),
             ?_assertEqual(ok, see_db_srv:queue(<<"https://pl.wikipedia.org/wiki/Wikipedia:Strona_główna"/utf8>>))
     end}.

when_page_returned_by_next_is_not_visited_in_time__it_is_queued_again_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(Pid) ->
             Self = self(),
             Ref = make_ref(),
             meck:expect(storage_mock, get_unvisited, fun() -> {ok, ?URL} end),
             meck:expect(storage_mock, set_unvisited, fun(?URL) -> Self ! Ref end),
             {ok, ?URL} = see_db_srv:next(),
             trigger_timeout(Pid),
             receive
                 SentRef -> ?_assertEqual(Ref, SentRef)
             after
                 1000 ->
                     ?assert(timeout)
             end
     end}.

when_visited_is_called__update_url_in_the_storage_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(Pid) ->
             meck:expect(see_text, extract_words, fun(words) -> extract_words end),
             meck:expect(storage_mock, update_url, [{[?URL, binary, content], ok},
                                                    {[?URL, ?TITLE, extract_words], ok}]),

             [?_assertEqual(ok, see_db_srv:visited(?URL, {data, ?TITLE, words})),
              ?_assertEqual(ok, see_db_srv:visited(?URL, content)),
              ?_assertEqual([], find_timer(Pid))]
     end}.

when_phrase_is_empty__search_returns_empty_list_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             ?assert_search_result([], <<"">>)
     end}.

when_word_is_not_present__search_returns_empty_list_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Word = <<"dsfds">>,
             meck:expect(storage_mock, get_pages_from_index, [{[Word], sets:new()}]),
             ?assert_search_result([], Word)
     end}.

when_all_words_are_present_on_one_page__search_returns_single_page_list_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Words = [<<"aaa">>, <<"bbb">>, <<"ccc">>],
             Page = 123,
             meck:expect(storage_mock, get_page, [{[Page], {?URL, ?TITLE}}]),
             meck:expect(storage_mock, get_pages_from_index, [{[Word], sets:from_list([Page])} || Word <- Words]),

             [?assert_search_result([{?URL, ?TITLE}],  <<"aaa">>),
              ?assert_search_result([{?URL, ?TITLE}],  <<"bbb">>),
              ?assert_search_result([{?URL, ?TITLE}],  <<"ccc">>),
              ?assert_search_result([{?URL, ?TITLE}],  <<"aaa bbb">>),
              ?assert_search_result([{?URL, ?TITLE}],  <<"aaa aaa aaa">>),
              ?assert_search_result([{?URL, ?TITLE}],  <<"aaa bbb ccc">>),
              ?assert_search_result([{?URL, ?TITLE}],  <<"aaa ccc bbb">>),
              ?assert_search_result([{?URL, ?TITLE}],  <<"ccc bbb aaa">>)]
     end}.

when_many_words_are_given__search_returns_pages_containing_all_of_them_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             meck:expect(storage_mock, get_pages_from_index, [{[<<"aaa">>], sets:from_list([1, 2])},
                                                              {[<<"bbb">>], sets:from_list([2, 3])},
                                                              {[<<"ccc">>], sets:from_list([1, 3])},
                                                              {[<<"ddd">>], sets:from_list([1, 2, 3])}]),

             meck:expect(storage_mock, get_page, [{[1], {?URL,  ?TITLE}},
                                                  {[2], {?URL2, ?TITLE2}},
                                                  {[3], {?URL3, ?TITLE3}}]),

             [?assert_search_result([{?URL, ?TITLE}, {?URL2, ?TITLE2}], <<"aaa">>),
              ?assert_search_result([{?URL, ?TITLE}, {?URL2, ?TITLE2}], <<"aaa aaa aaa">>),
              ?assert_search_result([{?URL2, ?TITLE2}], <<"aaa bbb">>),
              ?assert_search_result([], <<"aaa bbb ccc ddd">>),
              ?assert_search_result([{?URL3, ?TITLE3}], <<"bbb ccc ddd">>)]
     end}.
