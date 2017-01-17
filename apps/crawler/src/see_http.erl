-module(see_http).

-export([get_page/1]).

-define(CODE_OK, 200).
-define(CODE_MOVED, 301).
-define(CODE_FOUND, 302).

-define(TEXT_MIME, [<<"text/html">>, <<"text/plain">>]).

-define(MAX_LENGTH, 10000000). % 10MB

get_page(URL) ->
    Headers = [{"user-agent", "SEE (Search Engine in Erlang; https://github.com/tmaciejewski/see)"}],
    Payload = <<>>,
    Options = [{follow_redirect, false}],
    Response = hackney:request(get, URL, Headers, Payload, Options),
    handle_response(Response).

handle_response({ok, ?CODE_OK, Headers, BodyRef}) ->
    case is_text_page(Headers) of
        true ->
            hackney:body(BodyRef, ?MAX_LENGTH);
        false ->
            binary
    end;

handle_response({ok, ?CODE_MOVED, Headers, _}) ->
    HeaderDict = hackney_headers:new(Headers),
    case hackney_headers:get_value(<<"location">>, HeaderDict) of
        undefined ->
            {error, Headers};
        Location ->
            {redirect, Location}
    end;

handle_response({ok, ?CODE_FOUND, Headers, _}) ->
    HeaderDict = hackney_headers:new(Headers),
    case hackney_headers:get_value(<<"location">>, HeaderDict) of
        undefined ->
            {error, Headers};
        Location ->
            {redirect, Location}
    end;

handle_response({ok, Code, Headers, _}) ->
    {error, {Code, Headers}};

handle_response({error, Reason}) ->
    {error, Reason}.

is_text_page(Headers) ->
    HeaderDict = hackney_headers:new(Headers),
    case hackney_headers:get_value(<<"content-type">>, HeaderDict) of
        undefined ->
            false;
        ContentType ->
            MIME = hd(binary:split(ContentType, <<";">>, [trim_all])),
            lists:member(MIME, ?TEXT_MIME)
    end.
