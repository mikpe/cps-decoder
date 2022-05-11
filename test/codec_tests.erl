%%% -*- erlang-indent-level: 2 -*-
%%%
%%% Copyright 2021-2022 Mikael Pettersson
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(codec_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NR_TERMS, 10000).
-define(TERM_DEPTH, 6).
-define(TIMEOUT_SECONDS, 60).

codec_test_() ->
  {timeout, ?TIMEOUT_SECONDS, fun codectest/0}.

codectest() ->
  Nr = ?NR_TERMS,
  Depth = ?TERM_DEPTH,
  Encoded =
    lists:map(
      fun(Term) ->
        {Term, encode:encode(Term)}
      end,
      generate:generate(Nr, Depth)),
  lists:foreach(
    fun(Decoder) ->
      lists:foreach(
        fun({Term, EncBin}) ->
          ?assertEqual(Term, Decoder(EncBin))
        end,
        Encoded)
    end,
    [ fun decode_v1:decode/1
    , fun decode_v2:decode/1
    , fun decode_v3:decode/1
    , fun decode_v4:decode/1
    ]).
