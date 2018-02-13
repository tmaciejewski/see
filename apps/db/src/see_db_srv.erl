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

-record(state, {storage, rank, timers = maps:new(), domain_filter = none, visiting_timeout = 30000}).

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
    Rank = proplists:get_value(rank, Options),
    Storage = proplists:get_value(storage, Options),
    case Storage:start() of
        ok ->
            case proplists:get_value(domain_filter, Options) of
                undefined ->
                    {ok, #state{rank = Rank, storage = Storage, domain_filter = none}};
                DomainFilter when is_list(DomainFilter) ->
                    {ok, #state{rank = Rank, storage = Storage, domain_filter = DomainFilter}};
                _ ->
                    {stop, wrong_domain_filter}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

terminate(_, #state{storage = Storage}) ->
    Storage:stop().

handle_cast({visited, URL, {data, Title, Data}}, State = #state{storage = Storage, timers = Timers}) ->
    cancel_timer(URL, Timers),
    Words = see_text:extract_words(Data),
    Storage:update_url(URL, Title, Words),
    error_logger:info_report([{url, URL}, {title, Title}]),
    {noreply, State#state{timers = maps:remove(URL, Timers)}};

handle_cast({visited, URL, Content}, State = #state{storage = Storage, timers = Timers}) ->
    cancel_timer(URL, Timers),
    Storage:update_url(URL, binary, Content),
    {noreply, State#state{timers = maps:remove(URL, Timers)}}.

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

handle_call(next, _, State = #state{storage = Storage, timers = Timers}) ->
    case Storage:get_unvisited() of
        {ok, URL} ->
            {ok, Timer} = timer:send_after(State#state.visiting_timeout, {visiting_timeout, URL}),
            {reply, {ok, URL}, State#state{timers = maps:put(URL, Timer, Timers)}};
        nothing ->
            {reply, nothing, State}
    end;

handle_call({search, Query}, _, State = #state{storage = Storage, rank = Rank}) ->
    Words = see_text:extract_words(Query),
    AllPages = merge_page_lists([Storage:get_pages_from_index(Word) || Word <- Words]),
    RankedPages = [{Page, -Rank:rank(Page, Words, Storage)} || Page <- AllPages],
    Result = lists:sublist(lists:keysort(2, RankedPages), ?MAX_RESULTS),
    ResultURLs = [Storage:get_page(Id) || {Id, _} <- Result],
    error_logger:info_report([{query, Query}, {results, ResultURLs}]),
    {reply, ResultURLs, State}.

handle_info({visiting_timeout, URL}, State = #state{storage = Storage, timers = Timers}) ->
    Storage:set_unvisited(URL),
    {noreply, State#state{timers = maps:remove(URL, Timers)}}.

code_change(_OldVsn, State, _) ->
    {ok, State}.

%----------------------------------------------------------

parse_url(URL) ->
    case catch(hackney_url:normalize(hackney_url:urldecode(URL))) of
        ParsedURL = #hackney_url{path = <<$/, Path/binary>>} ->
            case filename:safe_relative_path(Path) of
                unsafe ->
                    error;
                [] ->
                    {ok, ParsedURL#hackney_url{path = <<$/>>, fragment = <<>>}};
                SimplifiedPath ->
                    {ok, ParsedURL#hackney_url{path = <<$/, SimplifiedPath/binary>>, fragment = <<>>}}
            end;
        _ ->
            error
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

cancel_timer(URL, Timers) ->
    case maps:get(URL, Timers, no_key) of
        no_key ->
            ok;
        Timer ->
            timer:cancel(Timer)
    end.
