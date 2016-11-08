# SEE

SEE (or see, whatever) is a simple search engine written in Erlang. So
far it can crawl through web indexing new pages into ets table. It's split
into two applications:

* `see_db` managing search index. There could be only one node running this application
* `see_crawler` responsible for crawling the web. There may be many nodes runnign this application.

## Starting see_db node

This will start `see_db` node with `-sname db`:

```bash
cd db
./start_db_node
```

By default the web interface is available at `http://localhost:8888`.

## Starting see_crawler node

This will create `see_crawler` node with crawlers geting URLs from the `see_db` node called `db`:

```bash
cd crawler
./start_crawler_node
```

Default number of crawlers is 1, but it can be changed by `crawler_num` parameter for `see_crawler` application.

## TODO

Things not yet implemented:
* stemming
* ranking
* distributed index storage
* persistent index storage
* periodically updating already visited pages
