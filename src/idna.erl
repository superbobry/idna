%% @doc Punycode implementation for Erlang.

-module(idna).
-author("Tim Fletcher <tim@tfletcher.com>").
-author("Sergei Lebedev <superbobry@gmail.com>").

-behaviour(application).

-export([start/2, stop/1]).
-export([start/0, stop/0, to_ascii/1, from_ascii/1]).

-define(ACE_PREFIX, "xn--").

%% API

-spec to_ascii(nonempty_string()) -> nonempty_string().
to_ascii(Domain) ->
    to_ascii(string:tokens(ux_string:to_lower(Domain), "."), []).

-spec from_ascii(nonempty_string()) -> nonempty_string().
from_ascii(Domain) ->
    from_ascii(string:tokens(Domain, "."), []).

-spec start() -> ok.
start() ->
    application:start(ux).

-spec stop() -> ok.
stop() ->
    application:stop(ux).

%% Application callbacks
start(_Type, _StartArgs) ->
    ok.

stop(_State) ->
    ok.

%% Helper functions

to_ascii([], Acc) ->
    lists:reverse(Acc);
to_ascii([Label|Labels], []) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label)));
to_ascii([Label|Labels], Acc) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label), [$.|Acc])).

label_to_ascii(Label) ->
    case lists:all(fun(C) -> xmerl_ucs:is_ascii(C) end, Label) of
        true  -> Label;
        false ->
            ?ACE_PREFIX ++ punycode:encode(ux_string:to_nfkc(Label))
    end.

from_ascii([], Acc) ->
    lists:reverse(Acc);
from_ascii([Label|Labels], []) ->
    from_ascii(Labels, lists:reverse(label_from_ascii(Label)));
from_ascii([Label|Labels], Acc) ->
    from_ascii(Labels, lists:reverse(label_from_ascii(Label), [$.|Acc])).

label_from_ascii(?ACE_PREFIX ++ Label) ->
	punycode:decode(Label);
label_from_ascii(Label) ->
	Label.
