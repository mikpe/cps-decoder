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
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Benchmark decoder performance

-module(bench).

-export([ stream/0
        , stream/1
        , test/0
        , test/2
        , test/3
        , test/4
        ]).

stream() ->
  stream(_Depth = 16).

stream(Depth) ->
  log("generating a binary with ~p packets", [1 bsl Depth]),
  Binary = stream_generate(Depth),
  lists:foreach(
    fun(DecodeFun) ->
      log("timing decoder ~p", [DecodeFun]),
      {Micros, _Ok} = timer:tc(fun() -> DecodeFun(Binary) end),
      log("decoder ~p took ~p micros", [DecodeFun, Micros])
    end,
    [ fun stream_v1:decode/1
    , fun stream_v2:decode/1
    ]).

stream_generate(0) -> [];
stream_generate(N) when is_integer(N), N > 0 ->
  list_to_binary(lists:duplicate(1 bsl N, <<8,"abcdefgh">>)).

test() ->
  test(_Nr = 100000, _Depth = 5).

test(Nr, Depth) ->
  test(Nr, Depth, decode_funs()).

test(Nr, Depth, DecodeFuns) ->
  test(Nr, Depth, DecodeFuns, export_seed()).

test(Nr, Depth, DecodeFuns, Seed) ->
  rand:seed(Seed),
  log("generating ~p terms with average depth ~p", [Nr, Depth]),
  Terms = generate:generate(Nr, Depth),
  log("encoding terms"),
  Encoded = lists:map(fun encode:encode/1, Terms),
  lists:foreach(
    fun(DecodeFun) ->
      log("timing decoder ~p", [DecodeFun]),
      {Micros, _Ok} = timer:tc(fun() -> lists:foreach(DecodeFun, Encoded) end),
      log("decoder ~p took ~p micros", [DecodeFun, Micros])
    end,
    DecodeFuns).

decode_funs() ->
  [ fun decode_v1:decode/1
  , fun decode_v2:decode/1
  , fun decode_v3:decode/1
  , fun decode_v4:decode/1
  ].

export_seed() ->
  case rand:export_seed() of
    undefined -> _ = rand:uniform(), rand:export_seed();
    Seed -> Seed
  end.

log(String) ->
  log("~s", [String]).

log(Fmt, Args) ->
  io:format("[~s] " ++ Fmt ++ "\n", [log_timestamp() | Args]).

log_timestamp() ->
  calendar:system_time_to_rfc3339(erlang:system_time(second)).
