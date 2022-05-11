# Copyright 2021-2022 Mikael Pettersson
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

SHELL := $(shell command -v bash)
REBAR3 := $(shell type -p rebar3 || echo ./rebar3)
REBAR3_GIT := https://github.com/erlang/rebar3.git
REBAR3_VSN := 3.18.0

compile: $(REBAR3)
	$(REBAR3) do compile, xref, dialyzer, eunit

distclean realclean: clean
	rm -f ./rebar3

clean: $(REBAR3)
	$(REBAR3) clean
	rm -rf _build

./rebar3:
	mkdir -p _build; \
	cd _build; \
	git clone --quiet $(REBAR3_GIT); \
	cd rebar3; \
	git checkout --quiet $(REBAR3_VSN); \
	./bootstrap; \
	mv rebar3 ../../; \
	cd ../..; \
	rm -rf _build/rebar3
