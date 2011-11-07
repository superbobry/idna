idna
====

Punycode (and IDNA) implementation for Erlang.


Quick start:
------------

Start with the usual [rebar](https://github.com/basho/rebar) boilerplate:

```bash
$ rebar get-deps compile
==> ux (compile)
==> erlang-idna (compile)
Compiled src/punycode.erl
Compiled src/idna.erl
```

Now, it's time for some action!

```erlang
$ erl -pa ebin deps/*/ebin
1> Domain = xmerl_ucs:from_utf8("президент.рф").
[1087,1088,1077,1079,1080,1076,1077,1085,1090,46,1088,1092]
2> idna:to_ascii(Domain).
"xn--d1abbgf6aiiy.xn--p1ai"
3> idna:from_ascii("xn--d1abbgf6aiiy.xn--p1ai").
[1087,1088,1077,1079,1080,1076,1077,1085,1090,46,1088,1092]
```

Reference material:
-------------------

- [RFC3490](http://www.ietf.org/rfc/rfc3490.txt) IDNA
- [RFC3492](http://www.ietf.org/rfc/rfc3492.txt) Punnycode
