-module(http).
-export([getPage/1, header/2]).

-define(TIMEOUT, 5000).

getPage(URL) ->
    case request(URL) of
        {ok, Response} ->
            case parseResponse(Response) of
                {Code, Headers, _} when (Code == 301) or (Code == 302) ->
                    {ok, Location} = header("Location", Headers),
                    getPage(Location);

                {Code, Headers, Content} ->
                    {ok, Code, Headers, Content}
            end;
        
        {error, Reason} -> {error, Reason}
    end.  

request(Host, Resource) ->
    string:join(["GET", Resource, "HTTP/1.1\r\n"
            "Host:", Host, "\r\n"
            "User-Agent: see crawler 0.1\r\n"
            "Accept: text/html,text/plain;q=0.9\r\n"
            "Connection: close\r\n"
            "\r\n"], " ").

header(_, []) -> error;
header(Name, [Header|Headers]) ->
    Div = string:chr(Header, $:),
    case string:sub_string(Header, 1, Div - 1) of
        Name -> {ok, string:strip(string:substr(Header, Div + 1))};
          _  -> header(Name, Headers)
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
            {Host, Resource} = 
                case string:chr(URL, $/) of
                     0  -> {URL, "/"};
                    Div ->
                        {string:sub_string(URL, 1, Div - 1),
                         string:substr(URL, Div)}
                end,                    
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
            {ok, receiveData(Socket, [])};
        {error, Reason} ->
            error_logger:error_report(Reason),
            {error, Reason}
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
