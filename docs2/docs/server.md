---
id: server
title: Server
---

The interpreter can also be accessed via a very simple HTTP API. The
API allows posting queries and obtaining responses. Additionally, a
Websocket is offered that clients can subscribe to in order to be
notified by the server when query responses are available.

The server can be spawned as follows:

```
joern --server
```

This will spawn a web server on port 8080 with the following
functionality: 


| Route  | Method | POST Body         | Response Body     |
| ------ | ------ | ------------      | -------------     |
| /query | POST   | {"query": $query} | {"uuid": "$uuid"} |
| /query?uuid=$uuid | GET    |  -     | {"success": "{true, false}", "stdout": "$stdout", "stderr" : "$stderr"} |


where `$query` is the query to be executed, `$uuid` is an id assigned
to the query by the server upon receipt, and `$stdout` and `$stderr`
is the data written by the interpreter to standard output and standard
error respectively in response to the query.

If a response for a query is not yet available, the response body is
of the form `{"success" : "false", ...} ` and the client is expected
to ask for the response at a later point in time. For interactive use
cases, this polling approach may be problematic. In this case, the
client can subscribe to a Websocket offered via

```
ws://$hostname:8080/connect
```

where `$hostname` is the name of the host on which the server is
running. Once subscribed, uuids of completed queries are reported on
the web socket.


:::note
Note that the server exclusively implements remote access to an
interpreter, it does not implement security mechanisms such as
authentication or sandboxing. As such, it is not to be considered a
security boundary and sandboxing and authentication must be achieved
via other means in production environments.
:::
