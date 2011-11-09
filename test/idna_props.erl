-module(idna_props).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Generators.

unicode_char() ->
    %% Non ASCII part of UTF-8 table:
    %% - Cyrillic
    %% - Curillic Supplementary
    integer(16#0400, 16#052F).

unicode_label() ->
    ?SUCHTHAT(Label,
              non_empty(list(unicode_char())),
              length(Label) < 64).

unicode_hostname() ->
    ?SUCHTHAT(Hostname,
              ?LET(Labels,
                   non_empty(list(unicode_label())),
                   string:join(Labels, ".")),
              length(Hostname) < 256).

mixed_hostname() ->
    ?SUCHTHAT(Hostname,
              ?LET(Labels,
                   non_empty(list(oneof([unicode_label(),
                                         proper_stdgen:label()]))),
                   string:join(Labels, ".")),
              length(Hostname) < 256).

%%% Properties.

prop_from_to() ->
    ?FORALL(RawMsg, oneof([proper_stdgen:hostname(),  %% plain ASCII.
                           unicode_hostname(),        %% Russian.
                           mixed_hostname()]),        %% Both.
            begin
                Msg = try
                          xmerl_ucs:from_utf8(RawMsg)
                      catch exit:_ -> RawMsg
                      end,

                idna:from_ascii(idna:to_ascii(Msg)) =:= ux_string:to_lower(Msg)
            end).

proper_test_() ->
    {timeout, 600,
     ?_assertEqual([], proper:module(idna_props, [{to_file, user},
                                                  {numtests, 5000}]))}.
