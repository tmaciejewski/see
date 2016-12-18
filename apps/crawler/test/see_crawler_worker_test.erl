-module(see_crawler_worker_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, "url").

trigger_timeout(Pid) ->
    Pid ! timeout.

start_crawler() ->
    meck:new(see_db_srv, [non_strict]),
    meck:new(see_http),
    Options = [{db_node, node()}, {worker_timeout, 3000}],
    {ok, Pid} = see_crawler_worker:start_link(Options),
    ?assert(is_pid(Pid)),
    Pid.

stop_crawler(Pid) ->
    ?assert(meck:validate(see_db_srv)),
    ?assert(meck:validate(see_http)),
    meck:unload(see_db_srv),
    meck:unload(see_http),
    see_crawler_worker:stop(Pid).

when_no_next_url__do_nothing__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             meck:expect(see_db_srv, next, fun() -> nothing end),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_next_url_is_error__call_visited_with_undefined__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             Reason = "error reason",
             meck:expect(see_db_srv, next, [{[], {ok, ?URL}}]),
             meck:expect(see_http, get_page, [{[?URL], {error, Reason}}]),
             meck:expect(see_db_srv, visited, [{[?URL, {error, Reason}], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_next_url_is_binary__call_visited_with_binary__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             meck:expect(see_db_srv, next, [{[], {ok, ?URL}}]),
             meck:expect(see_http, get_page, [{[?URL], binary}]),
             meck:expect(see_db_srv, visited, [{[?URL, binary], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_next_url_is_normal_page__call_visited_with_content__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             Links = ["link1", "link2"],
             Content = "page word",
             meck:expect(see_db_srv, next, [{[], {ok, ?URL}}]),
             meck:expect(see_http, get_page, [{[?URL], {ok, Content, Links}}]),
             meck:expect(see_db_srv, visited, [{[?URL, {data, Content}], ok}]),
             meck:expect(see_db_srv, queue, [{["link1"], ok}, {["link2"], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_next_url_is_redirect__call_visited_with_redirect_url__test_() ->
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
