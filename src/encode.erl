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
%%% Encodes a subset of Erlang terms to binaries.

-module(encode).

-export([ encode/1
        ]).

-include("codec.hrl").

%% To simplify things we require the size of variable-sized data to fit in one byte.
-define(MAX_SIZE, ((1 bsl 8) - 1)).

-spec encode(term()) -> binary().
encode(Term) ->
  case Term of
    [] -> encode_nil();
    [Head | Tail] -> encode_list(Head, Tail);
    Tuple when is_tuple(Tuple) -> encode_tuple(Tuple);
    Bin when is_binary(Bin) -> encode_binary(Bin);
    Int when is_integer(Int) -> encode_integer(Int)
  end.

encode_nil() ->
  <<?TAG_NIL:8>>.

encode_list(Head, Tail) ->
  <<?TAG_LIST:8, (encode(Head))/binary, (encode(Tail))/binary>>.

encode_tuple(Tuple) ->
  Sz = size(Tuple),
  true = Sz =< ?MAX_SIZE,
  encode_tuple(Tuple, 1, Sz, <<?TAG_TUPLE:8, Sz:8>>).

encode_tuple(Tuple, I, Sz, Acc) when I =< Sz ->
  Element = encode(element(I, Tuple)),
  encode_tuple(Tuple, I + 1, Sz, <<Acc/binary, Element/binary>>);
encode_tuple(_Tuple, _I, _Sz, Acc) ->
  Acc.

encode_binary(Bin) ->
  Sz = byte_size(Bin),
  true = Sz =< ?MAX_SIZE,
  <<?TAG_BINARY:8, Sz:8, Bin/binary>>.

encode_integer(Int) when Int >= 0 ->
  encode_uint(Int, ?TAG_POS_INT);
encode_integer(Int) when Int < 0 ->
  encode_uint(-Int, ?TAG_NEG_INT).

encode_uint(UInt, Tag) ->
  Bytes = uint_to_bytes(UInt),
  Sz = size(Bytes),
  true = Sz =< ?MAX_SIZE,
  <<Tag:8, Sz:8, Bytes/binary>>.

uint_to_bytes(UInt) ->
  uint_to_bytes(UInt, []).

uint_to_bytes(0, Bytes) ->
  list_to_binary(Bytes);
uint_to_bytes(UInt, Bytes) ->
  uint_to_bytes(UInt bsr 8, [(UInt band 255) | Bytes]).
