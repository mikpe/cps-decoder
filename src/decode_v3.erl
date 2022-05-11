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
%%% Decoder - V3, V2 made first-order via defunctionalization and closure conversion

-module(decode_v3).

-export([ decode/1
        ]).

-include("codec.hrl").

-spec decode(binary()) -> term().
decode(Binary) ->
  decode_v3_term(Binary, []).

decode_v3_term(Binary, Cont) ->
  case Binary of
    <<?TAG_NIL:8, Rest/binary>> -> decode_v3_nil(Rest, Cont);
    <<?TAG_LIST:8, Rest/binary>> -> decode_v3_list(Rest, Cont);
    <<?TAG_TUPLE:8, Rest/binary>> -> decode_v3_tuple(Rest, Cont);
    <<?TAG_BINARY:8, Rest/binary>> -> decode_v3_binary(Rest, Cont);
    <<?TAG_POS_INT:8, Rest/binary>> -> decode_v3_pos_int(Rest, Cont);
    <<?TAG_NEG_INT:8, Rest/binary>> -> decode_v3_neg_int(Rest, Cont)
  end.

decode_v3_cont(<<Rest/binary>>, [Cont1 | Cont2], Term) ->
  case Cont1 of
    hd ->
      decode_v3_term(Rest, [{tl, Term} | Cont2]);
    {tl, Head} ->
      decode_v3_cont(Rest, Cont2, [Head | Term]);
    {tuple, 1, Acc} ->
      %% We could avoid the reverse/1 by emitting the elements in reverse order,
      %% but that would break the intended use-case (sext).
      decode_v3_cont(Rest, Cont2, list_to_tuple(lists:reverse([Term | Acc])));
    {tuple, Sz, Acc} ->
      decode_v3_term(Rest, [{tuple, Sz - 1, [Term | Acc]} | Cont2])
  end;
decode_v3_cont(<<>>, [], Term) ->
  Term.

decode_v3_nil(<<Rest/binary>>, Cont) ->
  decode_v3_cont(Rest, Cont, []).

decode_v3_list(<<Rest/binary>>, Cont) ->
  decode_v3_term(Rest, [hd | Cont]).

decode_v3_tuple(<<0:8, Rest/binary>>, Cont) ->
  decode_v3_cont(Rest, Cont, {});
decode_v3_tuple(<<Sz:8, Rest/binary>>, Cont) ->
  decode_v3_term(Rest, [{tuple, Sz, []} | Cont]).

decode_v3_binary(<<Sz:8, Bin:Sz/binary, Rest/binary>>, Cont) ->
  decode_v3_cont(Rest, Cont, Bin).

decode_v3_pos_int(<<Sz:8, UInt:Sz/big-unsigned-integer-unit:8, Rest/binary>>, Cont) ->
  decode_v3_cont(Rest, Cont, UInt).

decode_v3_neg_int(<<Sz:8, UInt:Sz/big-unsigned-integer-unit:8, Rest/binary>>, Cont) ->
  decode_v3_cont(Rest, Cont, -UInt).
