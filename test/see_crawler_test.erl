-module(see_crawler_test).
-include_lib("eunit/include/eunit.hrl").
 
trigger_timeout(Pid) ->
    Pid ! timeout.

expect_calling_next_url(URL) ->
    meck:new(see_db),
    meck:expect(see_db, next, fun() -> URL end).

when_no_next_url__do_nothing() ->
    expect_calling_next_url(nothing),
    {ok, Pid} = see_crawler:start_link(),
    ?assert(is_pid(Pid)),
    trigger_timeout(Pid).
