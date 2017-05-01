-module(see_db_srv).
-behaviour(gen_server).

-include_lib("hackney/include/hackney_lib.hrl").

-export([start/1,
         start_link/1,
         stop/0,
         visited/2,
         queue/1,
         next/0,
         search/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(MAX_RESULTS, 100).

-record(state, {storage, domain_filter = none, visiting_timeout = 30000}).

start(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop() ->
    gen_server:call(?MODULE, stop).

visited(URL, Content) when is_binary(URL) ->
    gen_server:cast(?MODULE, {visited, URL, Content}).

queue(URL) when is_binary(URL) ->
    gen_server:call(?MODULE, {queue, URL}).

next() ->
    gen_server:call(?MODULE, next).

search(Query) when is_binary(Query) ->
    gen_server:call(?MODULE, {search, Query}).

%----------------------------------------------------------

init(Options) ->
    Storage = proplists:get_value(storage, Options),
    Storage:init(),
    case proplists:get_value(domain_filter, Options) of
        undefined ->
            {ok, #state{storage = Storage ,domain_filter = none}};
        DomainFilter when is_list(DomainFilter) ->
            {ok, #state{storage = Storage, domain_filter = DomainFilter}};
        _ ->
            {stop, wrong_domain_filter}
    end.

terminate(_, _) ->
    ok.

handle_cast({visited, URL, {data, Title, Data}}, State = #state{storage = Storage}) ->
    %TODO kill the timer
    Words = see_text:extract_words(Data),
    Storage:update_url(URL, Title, Words),
    error_logger:info_report([{url, URL}, {title, Title}]),
    {noreply, State};

handle_cast({visited, URL, Content}, State = #state{storage = Storage}) ->
    Storage:update_url(URL, Content),
    {noreply, State}.

handle_call(stop, _, State) ->
    {stop, shutdown, ok, State};

handle_call({queue, URL}, _, State = #state{storage = Storage}) ->
    case parse_url(URL) of
        {ok, ParsedURL} ->
            case filter_url(ParsedURL, State#state.domain_filter) of
                match ->
                    error_logger:info_report([{queued, ParsedURL}]),
                    Storage:add_url(hackney_url:unparse_url(ParsedURL)),
                    {reply, ok, State};
                nomatch ->
                    {reply, filter_mismatch, State}
            end;
        error ->
            {reply, url_error, State}
    end;

handle_call(next, _, State = #state{storage = Storage}) ->
    case Storage:get_unvisited() of
        {ok, URL} ->
            timer:send_after(State#state.visiting_timeout, {visiting_timeout, URL}),
            {reply, {ok, URL}, State};
        nothing ->
            {reply, nothing, State}
    end;

handle_call({search, Query}, _, State = #state{storage = Storage}) ->
    Words = see_text:extract_words(Query),
    PageLists = [Storage:get_pages_from_index(Word) || Word <- Words],
    ResultList = lists:sublist(merge_page_lists(PageLists), ?MAX_RESULTS),
    ResultPages = [Storage:get_page(Id) || Id <- ResultList],
    error_logger:info_report([{query, Query}, {results, ResultPages}]),
    {reply, ResultPages, State}.

handle_info({visiting_timeout, URL}, State = #state{storage = Storage}) ->
    Storage:set_unvisited(URL),
    {noreply, State}.

code_change(_OldVsn, State, _) ->
    {ok, State}.

%----------------------------------------------------------

parse_url(URL) ->
    case catch(hackney_url:normalize(hackney_url:urldecode(URL))) of
        {'EXIT', _Reason} ->
            error;
        ParsedURL ->
            {ok, ParsedURL#hackney_url{fragment = <<>>}}
    end.

filter_url(_, none) ->
    match;

filter_url(#hackney_url{netloc = Netloc}, DomainFilter) ->
    case re:run(Netloc, DomainFilter) of
        {match, _} ->
            match;
        nomatch ->
            nomatch
    end.

merge_page_lists([]) ->
    [];

merge_page_lists(PageLists) ->
    sets:to_list(sets:intersection(PageLists)).
