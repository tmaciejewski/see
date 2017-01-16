-module(see_db_srv).
-behaviour(gen_server).

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

-record(state, {domain_filter = none, visiting_timeout = 30000}).

-record(page, {id, url, title, content, last_visit = erlang:timestamp()}).
-record(index, {word, pages}).

start(Options) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Options, []).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop() ->
    gen_server:call(?MODULE, stop).

visited(URL, Content) when is_list(URL) ->
    gen_server:cast(?MODULE, {visited, URL, Content}).

queue(URL) when is_list(URL) ->
    gen_server:call(?MODULE, {queue, URL}).

next() ->
    gen_server:call(?MODULE, next).

search(Query) when is_binary(Query) ->
    gen_server:call(?MODULE, {search, Query}).

%----------------------------------------------------------

init(Options) ->
    ets:new(see_pages, [named_table, {keypos, #page.id}]),
    ets:new(see_index, [named_table, {keypos, #index.word}]),
    case proplists:get_value(domain_filter, Options) of
        undefined ->
            {ok, #state{domain_filter = none}};
        DomainFilter when is_list(DomainFilter) ->
            {ok, #state{domain_filter = DomainFilter}};
        _ ->
            {stop, wrong_domain_filter}
    end.

terminate(_, _) ->
    ok.

handle_cast({visited, URL, {data, Title, Data}}, State) ->
    case parse_url(URL) of
        {ok, ParsedURL} ->
            Words = see_text:extract_words(Data),
            Id = update_page(ParsedURL, Title, Words),
            lists:foreach(fun(Word) -> insert_to_index(Word, Id) end, Words),
            error_logger:info_report([{url, ParsedURL}, {title, Title}]),
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_cast({visited, URL, Content}, State) ->
    case parse_url(URL) of
        {ok, ParsedURL} ->
            update_page(ParsedURL, [], Content),
            {noreply, State};
        error ->
            {noreply, State}
    end.

handle_call(stop, _, State) ->
    {stop, shutdown, ok, State};

handle_call({queue, URL}, _, State) ->
    case parse_url(URL) of
        {ok, ParsedURL} ->
            case filter_url(ParsedURL, State#state.domain_filter) of
                ok ->
                    error_logger:info_report([{queued, ParsedURL}]),
                    queue_url(ParsedURL),
                    {reply, ok, State};
                error ->
                    {reply, error, State}
            end;
        error ->
            {reply, error, State}
    end;

handle_call(next, _, State) ->
    case ets:match_object(see_pages, #page{last_visit = null, _ = '_'}, 1) of
        {[Page = #page{url = URL}], _} ->
            timer:send_after(State#state.visiting_timeout, {visiting_timeout, Page}),
            ets:insert(see_pages, Page#page{last_visit = pending}),
            {reply, {ok, mochiweb_util:urlunsplit(URL)}, State};
        '$end_of_table' ->
            {reply, nothing, State}
    end;

handle_call({search, Query}, _, State) ->
    Words = see_text:extract_words(Query),
    PageLists = [get_pages(Word) || Word <- Words],
    Result = [get_page(Id) || Id <- merge_page_lists(PageLists)],
    error_logger:info_report([{query, Query}, {results, Result}]),
    {reply, Result, State}.

handle_info({visiting_timeout, Page}, State) ->
    ets:insert(see_pages, Page#page{last_visit = null}),
    {noreply, State}.

code_change(_OldVsn, State, _) ->
    {ok, State}.

%----------------------------------------------------------

parse_url(URL) ->
    {Schema, Netloc, Path, Query, _} = mochiweb_util:urlsplit(URL),
    parse_url(string:to_lower(Schema), string:to_lower(Netloc), Path, Query).

parse_url("http", Netloc, [], Query) ->
    {ok, {"http", Netloc, "/", Query, []}};

parse_url("http", Netloc, Path, Query) ->
    {ok, {"http", Netloc, http_uri:decode(Path), http_uri:decode(Query), []}};

parse_url(_, _, _, _) ->
    error.

filter_url(_, none) ->
    ok;

filter_url({_, Netloc, _, _, _}, DomainFilter) ->
    case re:run(Netloc, DomainFilter) of
        {match, _} ->
            ok;
        nomatch ->
            error
    end.

queue_url(URL) ->
    Id = erlang:phash2(URL),
    case ets:lookup(see_pages, Id) of
        [] ->
            ets:insert(see_pages, #page{id = Id, url = URL, last_visit = null});
        _ ->
            ok
    end.

update_page(URL, Title, Content) ->
    Id = erlang:phash2(URL),
    remove_page_from_index(Id),
    ets:insert(see_pages, #page{id = Id, url = URL, title = Title, content = Content}),
    Id.

remove_page_from_index(Id) ->
    case ets:lookup(see_pages, Id) of
        [#page{content = Words}] when is_list(Words) ->
            lists:foreach(fun(Word) -> remove_page_from_word(Word, Id) end, Words);
        _Other ->
            ok
    end.

remove_page_from_word(Word, Id) ->
    case ets:lookup(see_index, Word) of
        [] ->
            ok;
        [#index{pages = Pages}] ->
            ets:insert(see_index, #index{word = Word, pages = sets:del_element(Id, Pages)})
    end.

insert_to_index(Word, Id) ->
    case ets:lookup(see_index, Word) of
        [#index{pages = Pages}] ->
            ets:insert(see_index, #index{word = Word, pages = sets:add_element(Id, Pages)});
        [] ->
            ets:insert(see_index, #index{word = Word, pages = sets:from_list([Id])})
    end.

get_page(Id) ->
    [#page{title = Title, url = URL}] = ets:lookup(see_pages, Id),
    {mochiweb_util:urlunsplit(URL), Title}.

get_pages(Word) ->
    case ets:lookup(see_index, Word) of
        [] ->
            sets:new();
        [#index{pages = Pages}] ->
            Pages
    end.

merge_page_lists([]) ->
    [];

merge_page_lists(PageLists) ->
    sets:to_list(sets:intersection(PageLists)).
