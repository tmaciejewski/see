# SEE

SEE (or see, whatever) is a simple search engine written in Erlang. So
far it can crawl through web indexing new pages into ets table. It's split
into two applications:

* `see_db` managing search index. There could be only one node running this application
* `see_crawler` responsible for crawling the web. There may be many nodes runnign this application.

# Starting see_db node

```bash
cd db
rebar compile
erl -sname db -pa ebin -run see_db start
```

# Starting see_crawler node

```bash
cd crawler
rebar compile
erl -sname crawler -pa ebin -run inets -run see_crawler -see_crawler db_node DB_NODE@DN_HOST
```

`DB_NODE@DB_HOST` must be the name of the node running `see_db`. This will create a supervisor
with crawlers geting URLs from the `see_db` node. Default number of crawlers is 1, but it can
be changed by `crawler_num` parameter for `see_crawler` application.

# Starting crawling

To start crawling, add first URL to the queue on the `see_db` node:

```erlang
see_db_srv:queue("www.yahoo.com").
```

# Searching queries

When you type on the `see_db` node:

```erlang
see_db_srv:search("USA Obama").
```

It'll search pages, where both "USA" and "Obama" appears, and returns a list
of them in an order from highest number of occurrences to lowest.
