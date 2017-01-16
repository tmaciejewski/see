-module(see_crawler_worker_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, "http://url.com/").

trigger_timeout(Pid) ->
    Pid ! timeout.

start_crawler() ->
    meck:new(see_db_srv, [non_strict]),
    meck:new(see_http),
    meck:new(see_html),
    Options = [{db_node, node()}],
    {ok, Pid} = see_crawler_worker:start_link(Options),
    ?assert(is_pid(Pid)),
    Pid.

stop_crawler(Pid) ->
    ?assert(meck:validate(see_db_srv)),
    ?assert(meck:validate(see_http)),
    ?assert(meck:validate(see_html)),
    meck:unload(see_db_srv),
    meck:unload(see_http),
    meck:unload(see_html),
    see_crawler_worker:stop(Pid).

when_no_next_url__do_nothing__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             meck:expect(see_db_srv, next, fun() -> nothing end),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_getting_page_yields_error__call_visited_with_reason__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             Reason = "error reason",
             meck:expect(see_db_srv, next, [{[], {ok, ?URL}}]),
             meck:expect(see_http, get_page, [{[?URL], {error, Reason}}]),
             meck:expect(see_db_srv, visited, [{[?URL, {error, Reason}], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_page_is_binary__call_visited_with_binary__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             meck:expect(see_db_srv, next, [{[], {ok, ?URL}}]),
             meck:expect(see_http, get_page, [{[?URL], binary}]),
             meck:expect(see_db_srv, visited, [{[?URL, binary], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_page_is_redirect__call_visited_with_redirect_url__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             RedirectURL = "redirected url",
             meck:expect(see_db_srv, next, [{[], {ok, ?URL}}]),
             meck:expect(see_http, get_page, [{[?URL], {redirect, RedirectURL}}]),
             meck:expect(see_db_srv, visited, [{[?URL, {redirect, RedirectURL}], ok}]),
             meck:expect(see_db_srv, queue, [{[RedirectURL], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_page_is_text_page__call_visited_with_title_text_and_queue_links__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             Links = ["http://link1", "http://link2"],
             Content = "page content",
             Page = "page",
             Text = "page text",
             Title = "page title",
             meck:expect(see_db_srv, next, [{[], {ok, ?URL}}]),
             meck:expect(see_http, get_page, [{[?URL], {ok, Content}}]),
             meck:expect(see_html, parse, [{[Content], Page}]),
             meck:expect(see_html, title, [{[Page], Title}]),
             meck:expect(see_html, text, [{[Page], Text}]),
             meck:expect(see_html, links, [{[Page], Links}]),
             meck:expect(see_db_srv, visited, [{[?URL, {data, Title, Text}], ok}]),
             meck:expect(see_db_srv, queue, [{[Link], ok} || Link <- Links]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.


when_links_are_internal__convert_to_full_uri_test_() ->
    {foreach, fun start_crawler/0, fun stop_crawler/1,
     [fun(Pid) ->
              SubURL = ?URL ++ "/bar/sub.html",
              Links = ["relative/link", "/absolute/link"],
              Content = "page content",
              Page = "page",
              Text = "page text",
              Title = "page title",
              meck:expect(see_db_srv, next, [{[], {ok, SubURL}}]),
              meck:expect(see_http, get_page, [{[SubURL], {ok, Content}}]),
              meck:expect(see_html, parse, [{[Content], Page}]),
              meck:expect(see_html, title, [{[Page], Title}]),
              meck:expect(see_html, text, [{[Page], Text}]),
              meck:expect(see_html, links, [{[Page], Links}]),
              meck:expect(see_db_srv, visited, [{[SubURL, {data, Title, Text}], ok}]),
              meck:expect(see_db_srv, queue, [{[?URL ++ "bar/relative/link"], ok}, {[?URL ++ "absolute/link"], ok}]),
              trigger_timeout(Pid),
              ?_assert(is_pid(Pid))
      end,
      fun(Pid) ->
              Links = ["relative/link", "/absolute/link"],
              Content = "page content",
              Page = "page",
              Text = "page text",
              Title = "page title",
              meck:expect(see_db_srv, next, [{[], {ok, ?URL}}]),
              meck:expect(see_http, get_page, [{[?URL], {ok, Content}}]),
              meck:expect(see_html, parse, [{[Content], Page}]),
              meck:expect(see_html, title, [{[Page], Title}]),
              meck:expect(see_html, text, [{[Page], Text}]),
              meck:expect(see_html, links, [{[Page], Links}]),
              meck:expect(see_db_srv, visited, [{[?URL, {data, Title, Text}], ok}]),
              meck:expect(see_db_srv, queue, [{[?URL ++ "relative/link"], ok}, {[?URL ++ "absolute/link"], ok}]),
              trigger_timeout(Pid),
              ?_assert(is_pid(Pid))
      end]}.
