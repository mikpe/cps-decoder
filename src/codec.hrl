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
%%% Definitions shared between the encoder and decoders.

-ifndef(CODEC_HRL).
-define(CODEC_HRL, 1).

%% tags for encoded terms
-define(TAG_NIL, 0).
-define(TAG_LIST, 1).
-define(TAG_TUPLE, 2).
-define(TAG_BINARY, 3).
-define(TAG_POS_INT, 4).
-define(TAG_NEG_INT, 5).

-endif.
