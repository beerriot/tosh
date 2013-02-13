Tosh is a goofy gopher serving Riak content. It exposes a Gopher
protocol interface using
[Gopher Machine](https://github.com/beerriot/goma) on port `7070`.

## Configuration

Tosh connects to Riak using the Protocol Buffers interface. Where to
find that interface is configured by the `riak` setting in
`sys.config`. That setting is a proplist expecting `ip` and `port`
parameters. By default, it looks like this:

```erlang
 {tosh, [
         {riak, [
                 %% The IP on which to contact Riak
                 {ip, "127.0.0.1"},

                 %% The port on which to contact Riak's PB interface
                 {port, 8087}
                ]}
         ]},
```

## Building

To build Tosh, use rebar:

```shell
rebar get-deps compile generate
```

## Starting

To run Tosh, use the `tosh` script that was generated:

```shell
rel/tosh/bin/tosh console
```

## Accessing

To access Tosh, use `nc` or any other gopher client (like the
[OverbiteFF](https://addons.mozilla.org/en-us/firefox/addon/overbiteff/)
add-on for Firefox), and point it at `localhost:7070`.

Requesting the empty selector will get you a list of buckets:

```shell
nc localhost 7070

1c	/c	127.0.0.1	7070
1MyBucketName	/MyBucketName	127.0.0.1	7070
1FOO	/FOO	127.0.0.1	7070
1BAR	/BAR	127.0.0.1	7070

.
```

Requesting one of those bucket's selectors will get you a list of keys:

```shell
nc localhost 7070
/c
97	/c/7	127.0.0.1	7070
98	/c/8	127.0.0.1	7070
94	/c/4	127.0.0.1	7070
92	/c/2	127.0.0.1	7070
93	/c/3	127.0.0.1	7070
95	/c/5	127.0.0.1	7070
96	/c/6	127.0.0.1	7070
99	/c/9	127.0.0.1	7070
91	/c/1	127.0.0.1	7070
910	/c/10	127.0.0.1	7070

.
```

As you can see, all keys are binary resources. Requesting one will get
you its value:

```shell
nc localhost 7070
/c/1
hello
```
