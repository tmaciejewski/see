-module(see_http_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, "http://www.foo.com").

start() ->
    meck:new(httpc),
    ok.

stop(_) ->
    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

expect_http_request(URL, Result) ->
    meck:expect(httpc, request, [{[get, {URL, '_'}, '_', '_'], Result}]).

http_error_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Reason = "reason",
             expect_http_request(?URL, {error, Reason}),
             ?_assertEqual({error, Reason}, see_http:get_page(?URL))
     end}.

binary_blob_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Headers = [{"content-type", "application/octet-stream"}],
             Page = {{"HTTP/1.1", 200, "OK"}, Headers, ""},
             expect_http_request(?URL, {ok, Page}),
             ?_assertEqual(binary, see_http:get_page(?URL))
     end}.

code_301_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             RedirectURL = "redirect url",
             Headers = [{"location", RedirectURL}],
             Page = {{"HTTP/1.1", 301, "OK"}, Headers, []},
             expect_http_request(?URL, {ok, Page}),
             ?_assertEqual({redirect, RedirectURL}, see_http:get_page(?URL))
     end}.

code_302_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             RedirectURL = "redirect url",
             Headers = [{"location", RedirectURL}],
             Page = {{"HTTP/1.1", 302, "OK"}, Headers, []},
             expect_http_request(?URL, {ok, Page}),
             ?_assertEqual({redirect, RedirectURL}, see_http:get_page(?URL))
     end}.

unknown_code_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Headers = headers,
             Content = content,
             Code = 666,
             Page = {{"HTTP/1.1", Code, "OK"}, Headers, Content},
             expect_http_request(?URL, {ok, Page}),
             ?_assertEqual({error, {Code, Headers, Content}}, see_http:get_page(?URL))
     end}.

ok_code_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Content = "content",
             Headers = [{"content-type", "text/plain"}],
             Page = {{"HTTP/1.1", 200, "OK"}, Headers, Content},
             expect_http_request(?URL, {ok, Page}),
             ?_assertEqual({ok, Content}, see_http:get_page(?URL))
     end}.

url_encoding_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Content = "content",
             Headers = [{"content-type", "text/plain"}],
             Page = {{"HTTP/1.1", 200, "OK"}, Headers, Content},
             expect_http_request("http://localhost/foo/lorem%20ipsum", {ok, Page}),
             ?_assertEqual({ok, Content}, see_http:get_page("http://localhost/foo/lorem ipsum"))
     end}.
