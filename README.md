# SEE

SEE (or see, whatever) is a simple search engine written in Erlang. So
far it can crawl through web indexing new pages into ets table. It's split
into two applications:

* `see_db`
* `see_crawler`

## see_db

This application handles indexing and the web interface.
There could be only one node running this application.

Currently it stores data in ETS table, so it's not persistent.

To start the application, run `start_db_node`

Application parameters:
* `ip` (eg. `{0,0,0,0}`) -- web server ip address
* `port` (eg. `8888`) -- web server port
* `domain_filter` (eg. `"^localhost"`) -- regexp filter for URLs (useful for narrowing searching for only specific domain)

## see_crawler

This application is responsible for crawling the web.
There may be many nodes runnign this application.

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

* storing page title
* https support
* ranking
* stemming
* distributed index storage
* persistent index storage
* periodically updating already visited pages
