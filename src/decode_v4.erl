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
%%% Decoder - V4, V3 with continuation representation optimized

-module(decode_v4).

-export([ decode/1
        ]).

-include("codec.hrl").

%% the four different kinds of continuations
-define(V4_HD(Cont), {Cont}).
-define(V4_TL(Head, Cont), {Head, Cont}).
-define(V4_TUPLE(Sz, Acc, Cont), {Sz, Acc, Cont}).
-define(V4_STOP, {}).

-spec decode(binary()) -> term().
decode(Binary) ->
  decode_v4_term(Binary, ?V4_STOP).

decode_v4_term(Binary, Cont) ->
  case Binary of
    <<?TAG_NIL:8, Rest/binary>> -> decode_v4_nil(Rest, Cont);
    <<?TAG_LIST:8, Rest/binary>> -> decode_v4_list(Rest, Cont);
    <<?TAG_TUPLE:8, Rest/binary>> -> decode_v4_tuple(Rest, Cont);
    <<?TAG_BINARY:8, Rest/binary>> -> decode_v4_binary(Rest, Cont);
    <<?TAG_POS_INT:8, Rest/binary>> -> decode_v4_pos_int(Rest, Cont);
    <<?TAG_NEG_INT:8, Rest/binary>> -> decode_v4_neg_int(Rest, Cont)
  end.

decode_v4_cont(<<Rest/binary>>, Cont, Term) ->
  case Cont of
    ?V4_HD(Cont2) ->
      decode_v4_term(Rest, ?V4_TL(Term, Cont2));
    ?V4_TL(H, Cont2) ->
      decode_v4_cont(Rest, Cont2, [H | Term]);
    ?V4_TUPLE(1, Acc, Cont2) ->
      %% We could avoid the reverse/1 by emitting the elements in reverse order,
      %% but that would break the intended use-case (sext).
      decode_v4_cont(Rest, Cont2, list_to_tuple(lists:reverse([Term | Acc])));
    ?V4_TUPLE(Sz, Acc, Cont2) ->
      decode_v4_term(Rest, ?V4_TUPLE(Sz - 1, [Term | Acc], Cont2));
    ?V4_STOP ->
      decode_v4_stop(Rest, Term)
  end.

decode_v4_stop(<<_:0/binary>>, Term) ->
  Term.

decode_v4_nil(<<Rest/binary>>, Cont) ->
  decode_v4_cont(Rest, Cont, []).

decode_v4_list(<<Rest/binary>>, Cont) ->
  decode_v4_term(Rest, ?V4_HD(Cont)).

decode_v4_tuple(<<0:8, Rest/binary>>, Cont) ->
  decode_v4_cont(Rest, Cont, {});
decode_v4_tuple(<<Sz:8, Rest/binary>>, Cont) ->
  decode_v4_term(Rest, ?V4_TUPLE(Sz, [], Cont)).

decode_v4_binary(<<Sz:8, Bin:Sz/binary, Rest/binary>>, Cont) ->
  decode_v4_cont(Rest, Cont, Bin).

decode_v4_pos_int(<<Sz:8, UInt:Sz/big-unsigned-integer-unit:8, Rest/binary>>, Cont) ->
  decode_v4_cont(Rest, Cont, UInt).

decode_v4_neg_int(<<Sz:8, UInt:Sz/big-unsigned-integer-unit:8, Rest/binary>>, Cont) ->
  decode_v4_cont(Rest, Cont, -UInt).
