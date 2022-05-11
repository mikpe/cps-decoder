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
%%% Decoder - V2, V1 made non-recursive via a CPS-transform

-module(decode_v2).

-export([ decode/1
        ]).

-include("codec.hrl").

-spec decode(binary()) -> term().
decode(Binary) ->
  decode_v2_term(Binary, fun(_Rest = <<>>, Term) -> Term end).

decode_v2_term(Binary, Cont) ->
  case Binary of
    <<?TAG_NIL:8, Rest/binary>> -> decode_v2_nil(Rest, Cont);
    <<?TAG_LIST:8, Rest/binary>> -> decode_v2_list(Rest, Cont);
    <<?TAG_TUPLE:8, Rest/binary>> -> decode_v2_tuple(Rest, Cont);
    <<?TAG_BINARY:8, Rest/binary>> -> decode_v2_binary(Rest, Cont);
    <<?TAG_POS_INT:8, Rest/binary>> -> decode_v2_pos_int(Rest, Cont);
    <<?TAG_NEG_INT:8, Rest/binary>> -> decode_v2_neg_int(Rest, Cont)
  end.

decode_v2_nil(<<Rest/binary>>, Cont) ->
  Cont(Rest, []).

decode_v2_list(<<Rest/binary>>, Cont) ->
  decode_v2_term(
    Rest,
    fun(Rest1, Head) ->
      decode_v2_term(
        Rest1,
        fun(Rest2, Tail) ->
          Cont(Rest2, [Head | Tail])
        end)
    end).

decode_v2_tuple(<<0:8, Rest/binary>>, Cont) ->
  Cont(Rest, {});
decode_v2_tuple(<<Sz:8, Rest/binary>>, Cont) ->
  decode_v2_term(
    Rest,
    fun(Rest1, Elem1) -> decode_v2_tuple_elem_cont(Rest1, Elem1, Sz, [], Cont) end).

decode_v2_tuple_elem_cont(<<Rest/binary>>, Elem, Sz, Acc, Cont) ->
  NewAcc = [Elem | Acc],
  case Sz - 1 of
    0 ->
      %% We could avoid the reverse/1 by emitting the elements in reverse order,
      %% but that would break the intended use-case (sext).
      Cont(Rest, list_to_tuple(lists:reverse(NewAcc)));
    NewSz ->
      decode_v2_term(
        Rest,
        fun(Rest1, Elem1) -> decode_v2_tuple_elem_cont(Rest1, Elem1, NewSz, NewAcc, Cont) end)
  end.

decode_v2_binary(<<Sz:8, Bin:Sz/binary, Rest/binary>>, Cont) ->
  Cont(Rest, Bin).

decode_v2_pos_int(<<Sz:8, UInt:Sz/big-unsigned-integer-unit:8, Rest/binary>>, Cont) ->
  Cont(Rest, UInt).

decode_v2_neg_int(<<Sz:8, UInt:Sz/big-unsigned-integer-unit:8, Rest/binary>>, Cont) ->
  Cont(Rest, -UInt).
