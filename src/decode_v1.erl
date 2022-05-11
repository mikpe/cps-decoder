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
%%% Decoder - V1, recursive code mirroring the structure of the encoded terms

-module(decode_v1).

-export([ decode/1
        ]).

-include("codec.hrl").

-spec decode(binary()) -> term().
decode(Binary) ->
  {Term, _Rest = <<>>} = decode_v1_term(Binary),
  Term.

decode_v1_term(Binary) ->
  case Binary of
    <<?TAG_NIL:8, Rest/binary>> -> decode_v1_nil(Rest);
    <<?TAG_LIST:8, Rest/binary>> -> decode_v1_list(Rest);
    <<?TAG_TUPLE:8, Rest/binary>> -> decode_v1_tuple(Rest);
    <<?TAG_BINARY:8, Rest/binary>> -> decode_v1_binary(Rest);
    <<?TAG_POS_INT:8, Rest/binary>> -> decode_v1_pos_int(Rest);
    <<?TAG_NEG_INT:8, Rest/binary>> -> decode_v1_neg_int(Rest)
  end.

decode_v1_nil(<<Rest/binary>>) ->
  {[], Rest}.

decode_v1_list(Rest) ->
  {Head, Rest1} = decode_v1_term(Rest),
  {Tail, Rest2} = decode_v1_term(Rest1),
  {[Head | Tail], Rest2}.

decode_v1_tuple(<<Sz:8, Rest/binary>>) ->
  decode_v1_tuple(Sz, Rest, []).

decode_v1_tuple(0, <<Rest/binary>>, Acc) ->
  %% We could avoid the reverse/1 by emitting the elements in reverse order,
  %% but that would break the intended use-case (sext).
  {list_to_tuple(lists:reverse(Acc)), Rest};
decode_v1_tuple(Sz, <<Rest/binary>>, Acc) ->
  {Elem, Rest1} = decode_v1_term(Rest),
  decode_v1_tuple(Sz - 1, Rest1, [Elem | Acc]).

decode_v1_binary(<<Sz:8, Bin:Sz/binary, Rest/binary>>) ->
  {Bin, Rest}.

decode_v1_pos_int(<<Sz:8, UInt:Sz/big-unsigned-integer-unit:8, Rest/binary>>) ->
  {UInt, Rest}.

decode_v1_neg_int(<<Sz:8, UInt:Sz/big-unsigned-integer-unit:8, Rest/binary>>) ->
  {-UInt, Rest}.
