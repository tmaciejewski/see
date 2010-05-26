-module(crawler).
-export([getPage/1]).

-define(TIMEOUT, 3000).

getPage(URL) ->
    Response = request(URL),
    case parseResponse(Response) of
        {301, Headers, _} ->
            {ok, Location} = findHeader("Location", Headers),
            getPage(Location);

        {Code, _, Content} ->
            {Code, Content}
    end. 

request(Host, Resource) ->
    string:join(["GET", Resource, "HTTP/1.1\n"
            "Host:", Host, "\n"
            "User-Agent: see crawler 0.1\n"
            "Connection: close\n"
            "\n"], " ").

findHeader(_, []) -> error;
findHeader(Name, [Header|Headers]) ->
    Div = string:chr(Header, $:),
    case string:sub_string(Header, 1, Div - 1) of
        Name -> {ok, string:strip(string:substr(Header, Div + 1))};
          _  -> findHeader(Name, Headers)
    end.

parseResponse(Response) ->
    Div = string:str(Response, "\r\n\r\n"),
    [FirstLine|Headers] = string:tokens(string:substr(Response, 1, Div), "\r\n"),
    [_, Code | _] = string:tokens(FirstLine, " "),
    Content = string:substr(Response, Div + 4),
    {list_to_integer(Code),Headers, Content}.

parseURL(URL) ->
    case string:str(URL, "http://") of
        1 -> 
            parseURL(string:substr(URL, 8));
        _ ->
            Div = string:chr(URL, $/),
            Host = string:sub_string(URL, 1, Div - 1),
            Resource = string:substr(URL, Div),
            case string:tokens(Host, ":") of
                [Hostname] -> 
                    {Hostname, 80, Resource};
                [Hostname, Port] ->
                    {Hostname, list_to_integer(Port), Resource}
            end
    end.

request(URL) ->
    {Host, Port, Request} = parseURL(URL),
    case gen_tcp:connect(Host, Port, [], ?TIMEOUT) of
        {ok, Socket} ->
            gen_tcp:send(Socket, request(Host, Request)),
            receiveData(Socket, []);
        {error, Reason} ->
            error_logger:error_report(Reason)
    end.

receiveData(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            receiveData(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->
            lists:concat(lists:reverse(SoFar))
    after ?TIMEOUT ->
        lists:concat(lists:reverse(SoFar))
    end.
