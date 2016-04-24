-module(see_crawler_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, "foo.com").
-define(RequestURL, "http://foo.com").

trigger_timeout(Pid) ->
    Pid ! timeout.

start_crawler() ->
    meck:new(see_db),
    meck:new(httpc),
    {ok, Pid} = see_crawler:start_link(),
    ?assert(is_pid(Pid)),
    Pid.

stop_crawler(Pid) ->
    ?assert(meck:validate(see_db)),
    ?assert(meck:validate(httpc)),
    meck:unload(see_db),
    meck:unload(httpc),
    see_crawler:stop(Pid).

when_no_next_url__do_nothing__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             meck:expect(see_db, next, fun() -> nothing end),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_next_url_is_error__call_visited_with_undefined__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             meck:expect(httpc, request, [{[?RequestURL], {error, test}}]),
             meck:expect(see_db, next, [{[], {ok, ?URL}}]),
             meck:expect(see_db, visited, [{[?URL, error, test], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_next_url_is_binary__call_visited_with_binary__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             MIME = "application/octet-stream",
             Headers = [{"content-type", MIME}],
             Content = "content",
             Code = 200,
             Page = {{"HTTP/1.1", Code, "OK"}, Headers, Content},

             meck:expect(httpc, request, [{[?RequestURL], {ok, Page}}]),
             meck:expect(see_db, next, [{[], {ok, ?URL}}]),
             meck:expect(see_db, visited, [{[?URL, binary, MIME], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_next_url_is_text__call_visited_with_content__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             Headers = [{"content-type", "text/plain"}],
             Content = "page content",
             Words = string:tokens(Content, " "),
             Code = 200,
             Page = {{"HTTP/1.1", Code, "OK"}, Headers, Content},

             meck:expect(httpc, request, [{[?RequestURL], {ok, Page}}]),
             meck:expect(see_db, next, [{[], {ok, ?URL}}]),
             meck:expect(see_db, visited, [{[?URL, Code, Words], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.
