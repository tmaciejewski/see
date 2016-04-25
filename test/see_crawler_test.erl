-module(see_crawler_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, "foo.com").
-define(RequestURL, "http://foo.com").

trigger_timeout(Pid) ->
    Pid ! timeout.

start_crawler() ->
    meck:new(see_db),
    meck:new(httpc),
    meck:new(see_html),
    {ok, Pid} = see_crawler:start_link(),
    ?assert(is_pid(Pid)),
    Pid.

stop_crawler(Pid) ->
    ?assert(meck:validate(see_db)),
    ?assert(meck:validate(httpc)),
    ?assert(meck:validate(see_html)),
    meck:unload(see_db),
    meck:unload(httpc),
    meck:unload(see_html),
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
             meck:expect(see_db, visited, [{[?URL, {error, test}], ok}]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.

when_next_url_is_binary__call_visited_with_binary__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             Headers = [{"content-type", "application/octet-stream"}],
             Content = "content",
             Code = 200,
             Page = {{"HTTP/1.1", Code, "OK"}, Headers, Content},

             meck:expect(httpc, request, [{[?RequestURL], {ok, Page}}]),
             meck:expect(see_db, next, [{[], {ok, ?URL}}]),
             meck:expect(see_db, visited, [{[?URL, binary], ok}]),
             meck:expect(see_html, is_text, [{[Headers], false}]),
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
             meck:expect(see_db, visited, [{[?URL, Words], ok}]),
             meck:expect(see_html, is_text, [{[Headers], true}]),
             meck:expect(see_html, words, fun(Str) -> string:tokens(Str, " ") end),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.
