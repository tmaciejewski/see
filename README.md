# SEE

SEE (or see, whatever) is a simple search engine written in Erlang.
It provides web crawler, search engine and web frontend. It's split
up into two applications: `see_db`, `see_crawler`.

## see_db

`see_db` application handles indexing and web interface.
It allows choosing storage backend by `storage` option 
in `.app` file. Currently only ETS and Mnesia are supported.

To start the application, run `start_db_node` script.

Application parameters:
* `ip` (eg. `{0,0,0,0}`) -- web server ip address
* `port` (eg. `8888`) -- web server port
* `domain_filter` (eg. `"^localhost"`) -- regexp filter for URLs (useful for narrowing searching for only specific domain)
* `storage` -- storage module

### ETS storage

ETS storage is easy to set up, but it lacks persistance and distribution.
Only one `db` node is allowed, so the entire data must fit into RAM of a single machine.

To select ETS storage use `see_db_storage_ets` value as `storage` app option.

### Mnesia storage

Mnesia storage can be used to gain persistance and distribution. There can 
be as many `db` nodes as needed, though it was only tested using a single node.
All tables are `disc_copy`, so it still must fit into RAM as table fragmentation 
is not yet implemented.

To select Mnesia storage use `see_db_storage_mnesia` value as `storage` app option.
Then you need to create schema and tables. To do it for a single node, run 
`rebar3 shell --sname db@localhost` (or other name if you use custom name) and type:

    (db@localhost)1> mnesia:create_schema([node()]).
    ok
    (db@localhost)2> see_db_storage_mnesia:start().        
    ok
    (db@localhost)3> see_db_storage_mnesia:create_tables().
    {atomic,ok}

And exit erlang shell.

## see_crawler

This application is responsible for crawling the web.
There may be many nodes running this application.

To start the application, run `start_crawler_node`.

Application parameters:
* `crawler_num` (eg. `1`) -- number of crawler workers
* `db_node` (eg. `'db@localhost'`) -- `see_db` node name

## Usage

By default the web interface is available at `http://localhost:8888` on `db` node. You need to add first
URL to begin crawling with.

Each crawler requests an unvisited URL from `db` node and visits it, extracting words (as they are) and links from the page,
and sends them back to `db` node. Words after normalization are saved into the index and links are inserted as
unvisited URLs.

## TODO

- [x] HTTPS support
- [ ] different encoding support (eg. iso-8859, cp-1250)
- [ ] td-idf ranking
- [x] Mnesia storage backend
- [ ] Amazon S3 storage backend
- [ ] PageRank
- [ ] stemming
- [ ] complex queries (phrases, logic operators, `inurl:`, `intitle:`, `site:`)
- [ ] periodically updating already visited pages
