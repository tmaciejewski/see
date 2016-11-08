# SEE

SEE (or see, whatever) is a simple search engine written in Erlang. So
far it can crawl through web indexing new pages into ets table. It's split
into two applications:

* `see_db` handles indexing and the interface. There could be only one node running this application.
* `see_crawler` is responsible for crawling the web. There may be many nodes runnign this application.

## Starting see_db node

This will start `see_db` node with `-sname db`:

```bash
cd db
./start_db_node
```

## Starting see_crawler node

This will create `see_crawler` node with crawlers geting URLs from the `see_db` node called `db`:

```bash
cd crawler
./start_crawler_node
```

Default number of crawlers is 1, but it can be changed by `crawler_num` parameter for `see_crawler` application.

## Usage

By default the web interface is available at `http://localhost:8888` on `db` node. You need to add first
URL to begin crawling with.

Each crawler requests an unvisited URL from `db` node and visits it, extracting words (as they are) and links from the page,
and sends them back to `db` node. Words after normalization are saved into the index and links are inserted as
unvisited URLs.

## TODO

Things not yet implemented:
* stemming
* ranking
* distributed index storage
* persistent index storage
* periodically updating already visited pages
