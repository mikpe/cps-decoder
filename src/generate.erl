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
%%% Generate random terms

-module(generate).

-export([ generate/2
        ]).

-spec generate(pos_integer(), pos_integer()) -> list(term()).
generate(Nr, Depth) when Nr > 0, Depth > 0 ->
  generate(Nr, Depth, []).

generate(0, _Depth, Terms) ->
  Terms;
generate(Nr, Depth, Terms) ->
  generate(Nr - 1, Depth, [generate_term(rand:uniform(Depth)) | Terms]).

generate_term(Depth) when Depth >= 0 ->
  case rand:uniform(5) of
    1 -> generate_list(Depth);
    2 -> generate_tuple(Depth);
    3 -> generate_binary(Depth);
    4 -> generate_pos_int(Depth);
    5 -> generate_neg_int(Depth)
  end.

generate_list(_Depth = 0) ->
  [];
generate_list(Depth) ->
  Width = rand:uniform(1 bsl Depth) - 1,
  generate_list(Width, Depth - 1, []).

generate_list(_Width = 0, _Depth, Acc) ->
  Acc;
generate_list(_Width = 1, Depth, Acc) ->
  Last = generate_term(Depth),
  Tail =
    %% randomize whether the list ends with nil or a dotted pair
    case generate_boolean() of
      true -> [Last];
      false -> Last
    end,
  Acc ++ Tail;
generate_list(Width, Depth, Acc) ->
  generate_list(Width - 1, Depth, [generate_term(Depth) | Acc]).

generate_tuple(_Depth = 0) ->
  {};
generate_tuple(Depth) ->
  Width = rand:uniform(1 bsl Depth) - 1,
  generate_tuple(Width, Depth - 1, []).

generate_tuple(_Width = 0, _Depth, Acc) ->
  list_to_tuple(Acc);
generate_tuple(Width, Depth, Acc) ->
  generate_tuple(Width - 1, Depth, [generate_term(Depth) | Acc]).

generate_binary(Depth) ->
  Width = rand:uniform(1 bsl Depth) - 1,
  generate_binary(Width, []).

generate_binary(_Width = 0, Acc) ->
  list_to_binary(Acc);
generate_binary(Width, Acc) ->
  generate_binary(Width - 1, [generate_byte() | Acc]).

generate_pos_int(Depth) ->
  Width = rand:uniform(1 bsl Depth),
  generate_pos_int(Width, 0).

generate_pos_int(_Width = 0, Acc) ->
  Acc;
generate_pos_int(Width, Acc) ->
  generate_pos_int(Width - 1, (Acc bsl 8) bor generate_byte()).

generate_neg_int(Depth) ->
  -generate_pos_int(Depth).

generate_byte() ->
  rand:uniform(256) - 1.

generate_boolean() ->
  %% the rand manual page recommends this recipe for random booleans
  rand:uniform(16) > 8.
