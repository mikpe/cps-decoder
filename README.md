# cps-decoder
Using Continuation-Passing Style to optimize recursive decoding of binaries in Erlang.

The Erlang VM implements an optimization for binary decoders written in a streaming style,
but which isn't applicable for decoders with a recursive structure. This post explains how
two program transformations, continuation-passing style and defunctionalization, enable
recursive decoders to also benefit from the optimization.

## Background

This investigation was inspired by a presentation at Code BEAM America 2021, "When you have to make it faster",
where the authors included descriptions of code changes that improved the performance of binary matching in
their streaming packet decoder. Such decoders typically work by looking at leading portions of a binary,
inspecting tag and/or size fields, extracting a sub binary ("the packet"), and then continuing to decode the
rest of the original binary:

```
decode(<<>>) -> ok;
decode(Binary) ->
  Rest = decode_packet(Binary),
  decode(Rest).

decode_packet(Binary) ->
  <<Sz:8, Packet:Sz/binary, Rest/binary>> = Binary,
  dosomethingwith(Packet),
  Rest.
```

One of the key suggestions was to avoid **returning** the rest of the original binary for continued decoding,
and instead combine the two into the same function clause:

```
decode(<<>>) -> ok;
decode(<<Sz:8, Packet:Sz/binary, Rest/binary>>) ->
  dosomethingwith(Packet),
  decode(Rest).
```

On the author's PC, with Erlang/OTP-24.2.2, the second version is more than 2 times faster, and close to 3 times faster
for longer streams. Why is that?

If we put the first version in a module `stream_v1.erl` and compile it with `erlc +bin_opt_info` the compiler says:

```
> erlc +bin_opt_info stream_v1.erl
stream_v1.erl:28: Warning: BINARY CREATED: binary is returned from the function
%   28| decode_packet(Binary) ->
```

but if we put the second version in `stream_v2.erl` and compile that the compiler tells us something different:

```
> erlc +bin_opt_info stream_v2.erl
stream_v2.erl:26: Warning: OPTIMIZED: match context reused
%   26|   decode(Rest).
```

The key here is the mention of **match context** and the fact that it gets **reused**. This is a good thing.

(The "Warning" is not really a warning but an artifact of `+bin_opt_info` piggy-backing on
the compiler's warning infrastructure to emit this information.)

## Binaries

Binaries are just arrays of bytes, but they have a few different representations internally:

- real binaries which contain or refer to arrays of bytes, they exist in two different
  forms depending on their size and how their memory is managed
- **sub binaries** which represent slices (start and length) of real binaries

## Binary Matching (Bit Syntax)

The **bit syntax** allows binaries to be viewed not simply as arrays of bytes, but as
sequences of variable-length **bit fields**. Binary matching is a left-to-right process where
an input binary is decomposed into a sequence of bit fields, and possibly a "rest" binary.

For example, the following will decompose a sequence of 8-bit bytes into a list of 4-bit nibbles:

```
nibbles(<<>>) -> [];
nibbles(<<N:4,Rest/bitstring>>) -> [N | nibbles(Rest)].
...
% nibbles(<<16#12,16#34>>) = [1,2,3,4]
```

To perform binary matching, the implementation needs:

- an input binary (start and length)
- its current **bit** position in that input as it scans from left to right
- its original input position in case it needs to backtrack to try another clause

These values form the so-called **match contexts**.

## Match Contexts

A **match context** is similar to a **sub binary** in that it represents a slice of a binary,
except it is a strictly internal data structure used to optimize binary matching. In particular,
it is updated by side-effects rather than by creating new versions.

A binary match starts by initializing a new match context from the input binary, then performs
the matching which updates the match context as the input position moves from left to right over
the binary, and ends by discarding the match context. If the rest of the matched binary is returned
as an Erlang term, a new sub binary is created from the match context with that content.

Here's `decode_packet/1` from above with annotations showing these actions:

```
decode_packet(Binary) ->
  %% At the start of this match, a match context is initialized from Binary.
  <<Sz:8, Packet:Sz/binary, Rest/binary>> = Binary,
  %% At the end of this match, the match context has been updated to refer to Rest.
  dosomethingwith(Packet),
  %% Returning Rest requires creating a new sub binary from the match context.
  Rest.
```

An important optimization is **match context reuse**. This occurs when the rest of one binary
matching operation is used as input to another binary matching operation. In this case the
creation of a sub binary for the rest of the first match is omitted and the match context is
passed as-is from the first to the second match. The optimization requires that the
match context is **single-threaded**, which means that every time it is updated the previous
version (before the update) becomes unused. Currently the compiler can prove this when
match contexts are passed forward in tail-recursive function calls, but **not** when they are
**returned** from function calls.

Here's the improved `decode/1` from above with annotations showing these actions:

```
%% On the initial call to decode/1, a match context is initialized from the input binary.
%% On tail-calls from itself, the input is a match context ready to be used.
decode(<<>>) -> ok;
decode(<<Sz:8, Packet:Sz/binary, Rest/binary>>) ->
  %% At the end of this match, the match context has been updated to refer to Rest.
  dosomethingwith(Packet),
  %% Rest is passed forward to a function the compiler knows always starts with a
  %% binary match, therefore the match context is passed as-is without creating a
  %% new sub binary.
  decode(Rest).
```

This explains the difference in performance between the two versions of `decode/1` above.
The first version returns the rest of the matched binary for each packet, requiring the creation
of a new sub binary for each packet in the stream. The second version passes the rest forward
in a tail-call with no other reference to it, allowing the existing match context to be re-used
without creating temporary sub binaries.

## Use-case: decoding deep recursive structures

At Klarna we have a system which performs very frequent decoding of Erlang terms that have been
encoded as binaries. These terms are the keys in that system's database tables, and may contain
any combination of types except functions.

To implement the equivalent of **ordered set** tables, the keys are encoded to binaries using `sext`,
which is order-preserving: the lexicographical order of the binaries of two encoded keys is the same
as the Erlang term order of the two keys.

`sext` also has a **prefix key** property, which ensures that the encoding of a pattern term with
wildcards (`'_'`) to the right in its structure sorts before the encoding of any term which is an instance
of that pattern. For example, the following ordering relationships hold:

```
{1, a} < {1, b} < {2, a}
encode({1, '_'}) < encode({1, a}) < encode({1, b}) < encode({2, a})
```

The underlying storage engines allow enumerating entries in encoded key order, and also allow
starting such enumerations at or just after a given encoded key. Combined with `sext`-encoded
keys this allows us to implement **selects** on prefix keys efficiently.

A decoder for encoded recursive terms would normally follow the recursive structure of the
original term, e.g.:

```
-spec decode(binary()) -> term().
decode(Bin) ->
  {Term, _Rest = <<>>} = decode_v1_term(Bin),
  Term.

-spec decode_v1_term(binary()) -> {term(), binary()}.
decode_v1_term(Bin) ->
  case Bin of
    <<?TAG_NIL:8, Rest/binary>> -> decode_v1_nil(Rest);
    <<?TAG_LIST:8, Rest/binary>> -> decode_v1_list(Rest);
    ...
  end.

decode_v1_nil(Rest) ->
  {[], Rest}.

decode_v1_list(Rest) ->
  {Head, Rest1} = decode_v1_term(Rest),
  {Tail, Rest2} = decode_v1_term(Rest1),
  {[Head | Tail], Rest2}.

...
```

We can see that this structure will **not** admit the **match context reuse** optimization,
because in every binary match for a sub-term the rest is returned meaning a new sub binary
has to be created.

## Continuation-Passing Style (CPS)

Continuation-Passing Style is a way of writing code where functions do not return values,
instead functions take an additional function parameter called a **continuation**, and
"return" by applying that function in a tail-call. The opposite, where functions _do_
return values, is called **direct style**.

For example, a function to sum the elements in a tree could be written as follows in direct style:

```
sum({Left, Right}) -> sum(Left) + sum(Right);
sum(N) when is_integer(N) -> N.
```

To rewrite this in continuation-passing style, we pass in an additional continuation parameter,
replace returns with tail-calls to the continuation, and replace recursive calls with tail-calls
that pass in a new continuation:

```
sum_cps({Left, Right}, Cont) ->
  sum_cps(Left, fun(V1) -> sum_cps(Right, fun(V2) -> Cont(V1 + V2) end) end);
sum_cps(N, Cont) when is_integer(N) -> Cont(N).
```

To call a CPS function from conventional direct style code we need a shim and a dummy continuation:

```
sum(Tree) -> sum_cps(Tree, fun(V) -> V end).
```

**The benefit of CPS in our use case is that it allows us to rewrite the decoder
in a way that (eventually) enables the match context reuse optimization.**

Here's how the CPS decoder looks:

```
decode(Bin) -> % this is the shim between DS and CPS
  decode_v2_term(Bin, fun(_Rest = <<>>, Term) -> Term end).

decode_v2_term(<<Bin/binary>>, Cont) ->
  case Bin of
    <<?TAG_NIL:8, Rest/binary>> -> decode_v2_nil(Rest, Cont);
    <<?TAG_LIST:8, Rest/binary>> -> decode_v2_list(Rest, Cont);
    ...
  end.

decode_v2_nil(<<Rest/binary>>, Cont) ->
  Cont(Rest, []).

decode_v2_list(<<Rest/binary>>, Cont) ->
  decode_v2_term(
    Rest,
    fun(<<Rest1/binary>>, Head) ->
      decode_v2_term(
        Rest1,
        fun(<<Rest2/binary>>, Tail) ->
	  Cont(Rest2, [Head | Tail])
	end)
    end).

...
```

In theory this version should allow match context reuse since the match contexts are always passed forward
in tail-calls to functions that start with a binary match. However, compiling this version with `+bin_opt_info`
shows some problems (this is just an excerpt):

```
decode_v2.erl:43: Warning: BINARY CREATED: binary is used in call to {b_var,1} which doesn't support context reuse
%   43|   Cont(Rest, []).

decode_v2.erl:43: Warning: NOT OPTIMIZED: binary is used in fun call ({b_var,1})
%   43|   Cont(Rest, []).
```

The issue is that the continuations are function values, and the compiler is unable to prove that
they in fact would admit match context reuse.

## Defunctionalization

Defunctionalization is a program transformation where **function values** are replaced by plain non-functional
data and a statically known **apply** function which simulates calls to those function values. A function value
may in general have free variables, its **closure**, and defunctionalization often needs to include a
**closure conversion** step to replace closures with a concrete representation of the free variables.

Our continuations are sequences of fragments, so we may represent them
as lists where each element is an atom or tuple representing that fragment. If we apply this idea to the
CPS decoder above we arrive at:

```
decode(Bin) -> % this is the shim between DS and CPS
  decode_v3_term(Bin, []).

decode_v3_term(<<Bin/binary>>, Cont) ->
  case Bin of
    <<?TAG_NIL:8, Rest/binary>> -> decode_v3_nil(Rest, Cont);
    <<?TAG_LIST:8, Rest/binary>> -> decode_v3_list(Rest, Cont);
    ...
  end.

decode_v3_cont(<<Rest/binary>>, [Frag | Cont], Term) ->
  case Frag of
    hd ->
      decode_v3_term(Rest, [{tl, Term} | Cont]);
    {tl, Head} ->
      decode_v3_cont(Rest, Cont, [Head | Term]);
    ...
  end;
decode_v3_cont(<<>>, [], Term) -> Term.

decode_v3_nil(<<Rest/binary>>, Cont) ->
  decode_v3_cont(Rest, Cont, []).

decode_v3_list(<<Rest/binary>>, Cont) ->
  decode_v3_term(Rest, [hd | Cont]).

...

```

Compiling this version with `+bin_opt_info` shows that we finally are enabling match context reuse throughout:

```
src/decode_v3.erl:34: Warning: OPTIMIZED: match context reused
%   34|     <<?TAG_NIL:8, Rest/binary>> -> decode_v3_nil(Rest, Cont);

src/decode_v3.erl:35: Warning: OPTIMIZED: match context reused
%   35|     <<?TAG_LIST:8, Rest/binary>> -> decode_v3_list(Rest, Cont);

...

src/decode_v3.erl:45: Warning: OPTIMIZED: match context reused
%   45|       decode_v3_term(Rest, [{tl, Term} | Cont2]);

src/decode_v3.erl:47: Warning: OPTIMIZED: match context reused
%   47|       decode_v3_cont(Rest, Cont2, [Head | Term]);

...
```

## Optimizing the representation of continuations

In our benchmark the CPS decoder is 33% slower than the original recursive decoder,
but the defunctionalized CPS decoder is 37% faster than the original. However, more
improvements are possible. In particular, `decode_cont/3` currently has to inspect
both the top-level list and the first fragment to determine what to do.
In this example there are only three possible shapes for a continuation:

- the `stop` continuation, which has no components
- the `hd` continuation, which has one component: another continuation
- the `tl` continuation, which has two components: the head value and another continuation

We can represent these as tuples, of size 0, 1, and 2, respectively, which leads to the fourth and final version:

```
-define(CONT_STOP, {}).
-define(CONT_HD(Cont), {Cont}).
-define(CONT_TL(Head, Cont), {Head, Cont}).

decode(Bin) -> % this is the shim between DS and CPS
  decode_v4_term(Bin, ?CONT_STOP).

decode_v4_term(<<Bin/binary>>, Cont) ->
  case Bin of
    <<?TAG_NIL:8, Rest/binary>> -> decode_v4_nil(Rest, Cont);
    <<?TAG_LIST:8, Rest/binary>> -> decode_v4_list(Rest, Cont);
    ...
  end.

decode_v4_cont(<<Rest/binary>>, Cont0, Term) ->
  case Cont0 of
    ?CONT_HD(Cont) ->
      decode_v4_term(Rest, ?CONT_TL(Term, Cont));
    ?CONT_TL(Head, Cont) ->
      decode_v4_cont(Rest, Cont, [Head | Term]);
    ?CONT_STOP ->
      decode_v4_stop(Rest, Term)
    ...
  end.

decode_v4_stop(<<_:0/binary>>, Term) ->
  Term.

decode_v4_nil(<<Rest/binary>>, Cont) ->
  decode_v4_cont(Rest, Cont, []).

decode_v4_list(<<Rest/binary>>, Cont) ->
  decode_v4_term(Rest, ?CONT_HD(Cont)).

...

```

This improves performance by another 10%, resulting in just over 40% improvement
compared to the the original recursive decoder, which is a speedup of a factor of 1.7.
Not quite earth-shattering, but useful nevertheless.

## Summary

We have seen that it is possible to improve the performance of a recursive binary decoder
by replacing the recursion with iteration via a CPS transformation, replacing the higher-order
functions with first-order code via defunctionalization and closure conversion, and finally
optimizing the representation of continuations to reduce the cost of interpreting them.
In our sample decoder this gives a performance improvement of a factor of 1.7.

It would be awesome if the compiler could perform these optimizations automatically.
Doing so would require two things:

- the pattern of returning known-size tuples within a module needs to be recognized,
  allowing them to become multiple-value returns, avoiding the need to allocate and
  match on intermediate tuples (this alone would be a great improvement in general,
  not just in this binary decoder use case)
- returning match contexts to consumers within a module known to allow match context reuse
  needs to be recognized, just like passing them forward through tail-calls currently is

Both of these would require the compiler to reason about the possible continuations of
local functions and their properties, but should not require an actual CPS-transformation.

The code for these decoders and benchmarks is available
[on the author's github](https://github.com/mikpe/cps-decoder.git).

## References

[Continuation](https://en.wikipedia.org/wiki/Continuation)

[Continuation-Passing Style](https://en.wikipedia.org/wiki/Continuation-passing_style)

[Defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization)

[Erlang Efficiency Guide, Constructing and Matching Binaries](https://www.erlang.org/doc/efficiency_guide/binaryhandling.html)

[Ross Schlakjer, Optimizing Erlang Binary Matching](https://rhye.org/post/erlang-binary-matching-performance/)

[Sortable External format](https://github.com/uwiger/sext)

[https://www.markdownguide.org/cheat-sheet/]: #
