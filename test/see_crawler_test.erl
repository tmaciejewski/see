-module(see_crawler_test).
-include_lib("eunit/include/eunit.hrl").
 
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

when_got_next_url__visit_it__test_() ->
    {setup, fun start_crawler/0, fun stop_crawler/1,
     fun(Pid) ->
             URL = "foo.com",
             RequestURL = "http://" ++ URL,
             meck:expect(httpc, request, [{[RequestURL], {error, test}}]),
             meck:expect(see_db, next, [{[], {ok, URL}}]),
             meck:expect(see_db, visit, [
                                         {[URL, error, test], ok},
                                         {[URL, undefined, visiting], ok}
                                        ]),
             trigger_timeout(Pid),
             ?_assert(is_pid(Pid))
     end}.
