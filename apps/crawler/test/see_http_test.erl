-module(see_http_test).
-include_lib("eunit/include/eunit.hrl").

-define(URL, "http://www.foo.com").

start() ->
    meck:new(hackney),
    ok.

stop(_) ->
    ?assert(meck:validate(hackney)),
    meck:unload(hackney).

expect_http_request(URL, Result) ->
    meck:expect(hackney, request, [{[get, URL, '_', '_', '_'], Result}]).

http_error_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Reason = "reason",
             expect_http_request(?URL, {error, Reason}),
             ?_assertEqual({error, Reason}, see_http:get_page(?URL))
     end}.

no_content_type_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Headers = [],
             expect_http_request(?URL, {ok, 200, Headers, make_ref()}),
             ?_assertEqual(binary, see_http:get_page(?URL))
     end}.

binary_blob_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Headers = [{<<"Content-Type">>, <<"application/octet-stream">>}],
             expect_http_request(?URL, {ok, 200, Headers, make_ref()}),
             ?_assertEqual(binary, see_http:get_page(?URL))
     end}.

redirect_test_() ->
    {foreach, fun start/0, fun stop/1,
     [fun(_) ->
              RedirectURL = <<"redirect url">>,
              Headers = [{<<"Location">>, RedirectURL}],
              expect_http_request(?URL, {ok, 301, Headers, make_ref()}),
              ?_assertEqual({redirect, RedirectURL}, see_http:get_page(?URL))
      end,
      fun(_) ->
              RedirectURL = <<"redirect url">>,
              Headers = [{<<"Location">>, RedirectURL}],
              expect_http_request(?URL, {ok, 302, Headers, make_ref()}),
              ?_assertEqual({redirect, RedirectURL}, see_http:get_page(?URL))
      end,
      fun(_) ->
              RedirectURL = <<"redirect url">>,
              Headers = [{<<"other-header">>, <<"foo">>}],
              expect_http_request(?URL, {ok, 302, Headers, make_ref()}),
              ?_assertEqual({error, Headers}, see_http:get_page(?URL))
      end]}.

unknown_code_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Headers = headers,
             Code = 666,
             expect_http_request(?URL, {ok, Code, Headers, make_ref()}),
             ?_assertEqual({error, {Code, Headers}}, see_http:get_page(?URL))
     end}.

ok_code_test_() ->
    {setup, fun start/0, fun stop/1,
     fun(_) ->
             Content = <<"content">>,
             Headers = [{<<"Content-Type">>, <<"text/plain">>}],
             Ref = make_ref(),
             expect_http_request(?URL, {ok, 200, Headers, Ref}),
             meck:expect(hackney, body, [{[Ref, '_'], {ok, Content}}]),
             ?_assertEqual({ok, Content}, see_http:get_page(?URL))
     end}.
