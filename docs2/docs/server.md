---
id: server
title: Server
---

The interpreter can also be accessed via an HTTP API. Its primary jobs
are to (a) provide an interface to allow querying Code Property Graphs
from non-JVM-based programming languages, and (b) enabling clients
with limited computational resources to outsource CPG construction and
querying to a server machine.

The server can be spawned as follows:

```
joern --server
```

Or, with HTTP endpoints protected by basic authentication:

```
joern --server --server-auth-username username --server-auth-password password
```

A sample client is available for Python at:

https://github.com/ShiftLeftSecurity/cpgqls-client-python#example-usage


## For Developers

If you are interested in querying Code Property Graphs from your
favorite programming language and that language is not JVM-based and
not Python, then the following information on the server's API may be
of interest to you.

The HTTP API allows posting queries and obtaining
responses. Additionally, a Websocket is offered that clients can
subscribe to in order to be notified by the server when query
responses are available.


This will spawn a web server on port 8080 with the following
functionality: 


| Route  | Description  | Method | POST Body         | Response Body     |
| ------ | ------------ | ------ | ------------      | -------------     |
| /query | Submit query | POST   | {"query": $query} | {"uuid": "$uuid"} |
| /result?uuid=$uuid | Retrieve response | GET    |  -     | {"success": "{true, false}", "stdout": "$stdout", "stderr" : "$stderr"} |


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
interpreter, it does not implement sandboxing. As such, it is not
to be considered a security boundary and sandboxing must be achieved
via other means in production environments.
:::
