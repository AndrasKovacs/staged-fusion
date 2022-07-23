# staged-fusion

This is a small experiment about acceptably robust fusion in Haskell, using
typed Template Haskell. You can find examples in [Benchmark](Benchmark.hs).

## Introduction

By fusion we loosely mean program transformations which remove intermediate
data structures. The `Functor` laws for lists are the archetypal example:

    map id = id
    map f . map g = map (f . g)

By rewriting programs left-to-right along these rules, programs get
quite obviously better.

So why not just add these rules to a compiler? The biggest concern is that these
two rules are not nearly general enough, they don't cover enough opportunities
for fusion. There are many other list functions which are not covered.

The standard approach is to use alternative representations for lists where a
wide range of functions can be fused.

- In Rust, there's extensive library support for iterators. Functions written
  using iterators are usually fused, but users have to explicitly convert
  between lists (or arrays, etc.) and iterators.
- With standard GHC lists, there's an *implicit* conversion between plain lists
  and the fusible representation, which is not exposed to end users. Similar
  practices are used in `vector` and in pre-2.0 versions of `text`.

Unfortunately, in actual runtime code, fusible representations usually have
*worse* performance than plain data representations. So we want to ensure
that fusible representations are only used at compile time, and they don't
contaminate optimized code output.

Also unfortunately, there's is no nice & principled mechanism in GHC or Rust to
guarantee that fusible representations are restricted to compile time. It turns
out that the only reasonably robust solution is to explicitly write
code-generating code. In GHC we have (typed) Template Haskell for this purpose,
in Rust we have macros, but neither of these implementations are particularly
nice or principled. This repository uses TH nonetheless.

In GHC, standard list fusion relies on
- Inlining: fusion fails if certain definitions are not inlined.
- Rewrite rules: if these are applied in the wrong order, or inlining and
  rewriting is mixed in the wrong order, fusion fails.
- Definitions being in the right modules: fusion often [fails
  across module boundaries](https://github.com/sgraf812/foldr-build), because
  exporting jumbles the correct ordering of inlining and rewriting.
- Core simplification:
  - if the simplifier does not appropriately beta-reduce certain function
    applications, fusion fails. Remark: while in inlining we can use the `{-#
    INLINE #-}` pragma to order GHC around, there's no way to force the
    simplifier to do certain beta-reductions.
  - if the simplifier does not know enough about [call
    arities](https://www.joachim-breitner.de/publications/CallArity-TFP.pdf),
    fusion fails.
  - if the simplifier misses an essential [call pattern
    specialization](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.145.7980&rep=rep1&type=pdf),
    then *certain* kinds of fusion optimizations can partially fail. This is not
    quite as bad as most fusion failures, but it does perform much worse than
    hand-fused code.

It's no wonder that fusion is notoriously unreliable in Haskell. Just during the
writing of the benchmark file in this repo, I bumped into four fusion failures
in basic list-using code! See [`Benchmarks.hs`](Benchmarks.hs). For a recent
data point, the `text` package dropped fusion from its 2.0 release and achieved
dramatic performance improvements (partially as a result of dropping fusion).

I present here a fusion library with the following features:
  - It's far more robust than standard list fusion or `vector`. It's more robust
    than any fusion implementation that I've seen in Haskell. There are many
	libraries though that I have not seen.
  - It only requires `-O1` compilation. In contrast, `vector` fusion requires
    call pattern specialization which is enabled in `-O2`.  I find this to be an
    issue, because `-O2` is not common in Haskell code, and it is actively
    discouraged in some contexts, for example in Hackage warnings.
  - Explicitly marks streams as either *push* or *pull*. Push streams support
    monadic binding and appending but no zipping. Pull streams support zipping
    but no binding or appending. There's an efficient conversion from pull to
    push but not the other way around. This is certainly an extra complexity for
    users, but as a result we get a significant improvement in the range of
    definable fusing definitions, compared to sticking to just push (like GHC
    lists) or pull (like `vector`).
  - Uses typed TH. It's pretty noisy, and the module restrictions are
    annoying, but there isn't really any alternative.
  - The implementation is inspired by two-level type theory (2LTT), which is a
    more expressive and principled system for two-stage compilation than
    TH. Concretely, I try to pretend to work in 2LTT and translate 2LTT
	definitions to TH.

It's worth to give a quick overview of 2LTT before we get to the actual code.
It provides a good amount of conceptual clarity, and it gives us a normative
guideline to what kind of staged code should make sense.

## Two-level type theory (2LTT)

You can look at [my paper](https://andraskovacs.github.io/pdfs/2ltt.pdf) for
gory details, or my [tutorial file in the demo
implementation](https://github.com/AndrasKovacs/staged/blob/main/demo/examples/Tutorial.2ltt)
for an alternative intro which is aimed at beginners although it assumes some
familiarity with dependent types.

Let's assume that we import `Type` from `Data.Kind` in Haskell, and use that
instead of `*`.

In typed TH, for any type `a :: Type`, we have the type of expressions with type
`a`. In this repo, I call this type `Up a :: Type`, in reference to my 2LTT
paper, and also because it's a short name. So we have in `Up.hs`:

    import Language.Haskell.TH

    type Up = CodeQ

This brings us to the main difference between TH and 2LTT.

In TH, the type of expressions is still in `Type`, which means that we can
arbitrarily mix together types which are of the form `Up a` and types which are
not. I can blithely write `f :: Int -> Code Int` or even `forall a. a -> Code a`.

In 2LTT, we have two different `Type`-s, let's call them `Type0` and `Type1`. I
summarize 2LTT primitives below, using TH-like syntax.
- `Type0` is the type of runtime types. Runtime types can appear in code output,
  and so do programs with runtime types.
- `Type1` is the type of compile time ("static") types. They cannot appear in code
  output, and programs with static types can't either.
- `Up` lifts `a :: Type0` to `Up a :: Type1`. This means that `Up a`, the type of
  runtime expressions, is restricted to compile time.
- For `a :: Type0` and `t :: a`, we have `[|| t ||] :: Up a`. This is called *quoting*.
- For `t :: Up a`, we have `$$t :: a`. This is *splicing*.
- `Up`, quoting and splicing are the *only* ways to cross between `Type0` and `Type1`.
  All other operations and type formers stay entirely within `Type0` or `Type1`.
- Quotation is the inverse of splicing. This is more important in dependently
  typed programs, where quotes and splices can occur in types and the type checker
  has to compare types for definitional equality.

In 2LTT, we *only* have typing rules; there are no syntactic restrictions and
there is no distinction between top-level and local things.

In TH, we also have that `Up a` is restricted to compile time. However, because
the system is not typed precisely enough, we have *syntactic* and *scope-based*
restrictions on what we can do, and we have a distinction between *top-level*
and *local* things.

An example for a top-local disambiguation of stages in TH:

    e :: Up Bool
	e = let b = True in [||b||]

Here, `b` is computed at compile time, so when we use it in the quotation, it's
implicitly converted to `Up Bool` through the `Lift` typeclass.

    b :: Bool
	b = True

    e :: Up Bool
	e = [||b||]

Here, `b` is a top-level definition which appears in the code output, so
`[||b||]` is the `b` *identifier* itself as an expression.

Let's look at the same in 2LTT. Here, there is no single `Bool` type; we need
two types if we want to use `Bool` both at runtime and at compile time. So we
have `Bool0 :: Type0` and `Bool1 :: Type1`. We define a "serialization" function
first. Note that we call it `lower` even though in TH it's called `lift`.

    lower :: Bool1 -> Up Bool0
	lower True1  = [||True0||]
	lower False1 = [||False0||]

This could be overloaded using a typeclass, the same way as in TH.  Then the
definitions:

    e :: Up Bool0
	e = let b = True1 in [|| $$(lower b) ||]

However, since quoting and splicing are inverses, this is just

    e :: Up Bool0
	e = let b = True1 in lower b

We can move `b` to the top-level and nothing changes:

    b :: Bool1
    b = True1

	e :: Up Bool0
	e = lower b

The other code sample:

    b :: Bool0
	b = True0

	e :: Up Bool0
	e = [||b||]


### Staging for types

Perhaps the biggest weakness of TH is that we can't compute types at compile
time. In 2LTT, we can, because quoting and splicing can be used on types too.
For example, if I have `a :: Type0`, then I have `[||a||] :: Up Type0`. This
makes sense, because just like as in Haskell, I have that `Type0 :: Type0`. I
can also use dependent functions to abstract over `Up Type0`. Take the
compile-time identity function:

    id1 :: forall (a :: Type1). a -> a
	id1 x = x

Now, I can use `id1` on runtime expressions:

    foo :: Bool0
	foo = $$(id1 @(Up Bool0) [||True||])

This works out because `Up Bool0 :: Type1` and `[||True||] :: Up Bool0`.

It becomes a bit more interesting when I want to write an inlined version
of `map`, because there I have to abstract over runtime types.

    map' :: forall (a :: Up Type0)(b :: Up Type0). (Up $$a -> Up $$b) -> Up [$$a] -> Up [$$b]
	map' f xs = [|| let go :: [$$a] -> [$$b]
	                   go []     = []
	                   go (x:xs) = $$(f [||x||]) : go xs
	                in go $$xs ||]

Now, I can apply `map'` to `[||Bool0||] :: Up Type0`, which is a *type
expression*. In TH, there's no such thing as a type expression, and types are
handled in a rather fuzzy manner. We can rewrite the above in TH as follows, and
it typechecks:

    {-# language TemplateHaskell, RankNTypes, ScopedTypeVariables #-}

    import Up(Up)
    import Data.Kind

    map' :: forall (a :: Type) (b :: Type). (Up a -> Up b) -> Up [a] -> Up [b]
    map' f xs = [|| let go :: [a] -> [b]
	                    go []     = []
                        go (x:xs) = $$(f [||x||]) : go xs
                    in go $$xs ||]

However, if we try to use it, we find that it's broken. The following doesn't
typecheck:

    f :: [Int] -> [Int]
    f xs = $$(map' (\(x :: Up Int) -> [||$$x + (10::Int)||]) [||xs||])

The issue is that TH loses all scoping of scoped type variables during splicing
(because it has no conception of type expressions). In other words, we can't use
scoped type variables in quotes, so the fix is:

    map' :: forall (a :: Type) (b :: Type). (Up a -> Up b) -> Up [a] -> Up [b]
    map' f xs = [|| let go []     = []
                        go (x:xs) = $$(f [||x||]) : go xs
                    in go $$xs ||]

This definition works fine. If types can be inferred through value-level type
dependencies alone, without refering to scoped type variables in annotations,
then we're fine. Usually a few helper functions or proxies are enough to make
this work.

There are good and deep reasons to treat 2LTT as *the* correct theoretical
basis of two-stage compilation, and even when writing TH it makes sense to
pretend to work in 2LTT. Everything which is well-typed in 2LTT is necessarily
sensible and sound in TH, whenever it's possible to rewrite it in TH.


## Fold-based (push) fusion

Let's move on to push fusion now. This is one of the two popular fusible
representations. It's the one used in GHC for lists.

As I mentioned, standard list fusion converts from runtime lists to fusible
lists, does fusion on fusible lists, then converts them back to runtime lists.
I also mentioned that it's essential that fusible list expressions are fully
known statically so that they can be computed away at compile time. In standard
GHC list fusion, the best we can do is to hope that fusible list expressions are
statically known.

Whenever GHC *happens* to be able to fully fuse a program, the same program can
be written in 2LTT, possibly with some extra staging annotations, and in 2LTT we
get a *formal guarantee* that everything fuses.

Let's define the "push" representation. The type of statically known list
expressions is

    Up [a]

Now, we can use Church-encoding under the `Up` to get

    Up (forall (b :: Type0). (a -> b -> b) -> a -> b)

We know an interesting fact about `Up`: it distributes over `forall` and
function arrows. For example, `Up (a -> b)` is isomorphic to `Up a -> Up b`.
We can define the conversions:

    to :: Up (a -> b) -> Up a -> Up b
	to f a = [|| $$(f $$a) ||]

	from :: (Up a -> Up b) -> Up (a -> b)
	from f = [|| \a -> $$(f [||a||]) ||]

Using this property, we further transform the type to

    forall (b :: Up Type0). (Up a -> Up $$b -> Up $$b) -> Up a -> Up $$b

And that's it, this is a type of fusible lists, called `Push`:

    Push :: Up Type0 -> Type1
    Push a = forall (b :: Up Type0). (Up $$a -> Up $$b -> Up $$b) -> Up $$a -> Up $$b

Above, I insert extra splicing in `$$a`, because if I want to define `Push` as a
static type synonym, I can't abstract over `Type0`, only `Up Type0`, because
only the latter is a static type.

Why use this representation? Intuitively, if I have a value of `Up [a]`, it's
just a piece of dead syntax, and there's no general and principled way to
transform it. But if I have a value of `Push a`, it's actually a higher-order
*static function*, which can applied to arguments at compile time. From the code
generation perspective, `Push a` is a function which expects a code generator
function for the `cons` case and an expression for the `nil` case, and returns
an expression.

Let's look at conversions between `Push` and lists:

    fromList :: forall (a :: Up Type0). Up [$$a] -> Push a
	fromList xs c n = [|| let go []     = $$n
	                          go (x:xs) = $$(c [||x||] [||go xs||])
	                      in go $$xs ||]

    toList :: Push a -> Up [$$a]
	toList xs = xs (\x xs -> [|| $$x : $$ xs ||]) [|| [] ||]

The `map` function:

    map :: (Up $$a -> Up $$b) -> Push a -> Push b
	map f as c n = as (\a bs -> c (f a) bs) n

If you think of ` as :: Push a` as something which inlines `c` and `n` into a
loop, `map f as` inlines `\a bs -> c (f a) bs` into a loop. Example:

    foo :: [Int] -> [Int]
	foo xs = $$(
	  toList
	    (map (\x -> [|| $$x + 10 ||])
	      (map (\x -> [|| $$x + 10 ||])
	  	    (fromList [||xs||]))))

The two `map` applications both inline a bit more code in the `cons` case, and the
`toList` in the end inlines the actual list constructors. So we get as staging
output

    foo :: [Int] -> [Int]
	foo xs = let go []     = []
	             go (x:xs) = (x + 10) + 10 : go xs
	         in go xs

Let's drop the 2LTT syntax now and switch to actual Haskell. We can define the
code so far as

    {-# language TemplateHaskell, RankNTypes, ScopedTypeVariables, BlockArguments #-}

    module Push where

    import Up (Up)

    newtype Push a = Push {fold :: forall b. (Up a -> Up b -> Up b) -> Up b -> Up b}

    map :: (Up a -> Up b) -> Push a -> Push b
    map f as = Push \c n -> fold as (\a bs -> c (f a) bs) n

    fromList :: Up [a] -> Push a
    fromList as = Push \c n -> [||
      let go []     = $$n
          go (a:as) = $$(c [||a||] [||go as||])
      in go $$as ||]

    toList :: Push a -> Up [a]
    toList as = fold as (\a as -> [|| $$a : $$as ||]) [|| [] ||]


When we create a `Push` from some non-`Push` data, we usually give a "template"
for a recursive loop, and we inline more and more code in the template when we
further process the `Push`. We can loop over things other than lists, for
example count from one `Int` to another:

    range :: Up Int -> Up Int -> Push Int
    range lo hi = Push \c n -> [||
      let go lo hi | (lo :: Int) >= (hi :: Int) = $$n
                   | otherwise = $$(c [||lo||] [||go (lo + 1) hi||])
      in go $$lo $$hi ||]

In general, every function which can be efficiently defined on plain lists using
`foldr`, can be efficiently defined on `Push`. It follows that we *can't*
implement `zipWith` on `Push` efficiently:

    zipWith :: (Up a -> Up b -> Up c) -> Push a -> Push b -> Push c

We'll define `zipWith` instead on the "pull" representation a bit later.


## Sugar for `Up` functions

In the [`Up.hs`](Up.hs) module I repeat a bunch of definitions from `Prelude`,
but in a "lifted" way so that everything operates on `Up a` values. Using the
extra sugar, the previous `foo` example can be written as

    {-# language TemplateHaskell #-}

    module Notes where

    import Up(Up)
    import qualified Up as U

    import Push (Push)
    import qualified Push as P

    foo :: [Int] -> [Int]
    foo xs = $$(
      P.toList $
      P.map (U.+ 10) $
      P.map (U.+ 10) $
      P.fromList [||xs||])

Here, I have that `(U.+) :: U.Num a => Up a -> Up a -> Up a`. This again
handwaves the staging of types and `Num` instances in a sketchy way, but it
seems to work out. In 2LTT, this would be

    (U.+) :: forall (a : Up Type0). Up (Num $$a) => Up $$a -> Up $$a -> Up $$a

This implies that `a` and the `Num` instance are also statically known.


## Unfold-based (pull) fusion

We've seen that zipping is not supported by `Push`. We address this by defining
`Pull` which does support zipping and which can be efficiently converted to
`Push`.

`Pull` is the Church-encoding of the *colists*, the possibly infinite
coinductive lists. This corresponds to a state machine which contains a starting
state and a stepping function:

    data Pull a = forall s. Pull {seed :: Up s, step :: Up (s -> Step a s)}

where

    data Step a s = Stop | Yield a s

However, we can make `Step` fusible by using Church-coding and distributing `Up`,
the same way as we did with `Push`.

    type Step a s = forall b. Up b -> (Up a -> Up s -> Up b) -> Up b

    data Pull a = forall s. Pull {seed :: Up s, step :: Up s -> Step a s}

`Pull` works dually to `Push` in the following sense:

- We usually use recursion when we create a `Push` from non-`Push` things.
- We usually use recursion when we compute non-`Pull` things from `Pull`.

For example, a strict fold over a `Pull` runs the state machine until it stops:

    foldl :: (Up b -> Up a -> Up b) -> Up b -> Pull a -> Up b
    foldl f b (Pull seed step) = [||
      let go s b = seq b $$(step [||s||] [||b||] (\a s -> [||go $$s $$(f [||b||] a)||]))
      in  go $$seed $$b ||]

In the `step` call, the second argument handles the `Stop` case and the third
the `Yield` case. The `seq b` is needed because TH seems to sometimes ignore
bang patterns in quotes.

Conversion to `Push` is exactly the same thing as implementing `foldr`:

    pullToPush :: Pull a -> Push a
    pullToPush (Pull seed step) = Push \c n ->
      [|| let go s = $$(step [||s||] n (\a s -> c a [||go $$s||]))
          in go $$seed ||]

Mapping applies a function to every yielded value:

    map :: (Up a -> Up b) -> Pull a -> Pull b
	map f (Pull seed step) =
	  Pull seed (\s stop yield -> step s stop (\a s -> yield (f a) s))

With filtering we bump into a design question. How should we skip over filtered
things?  In `vector` and in other libraries, the solution is to have a different
definition of `Step`:

    data Step a s = Stop | Yield a s | Skip s

The, we take the appropriate fusible representation of `Up (Step a s)`. With
this, filtering can be defined as converting all `Yield`-s in the input stream
to `Skip`-s in the output, whenever the filtering predicate is false.

While `Skip` has some advantages, I think that it's significantly simpler to not
have it. Without `Skip`, filtering can be defined by tail-recursively skipping
over all filtered values in each step. We write a helper function first:

    find :: (Up a -> Up Bool) -> (Up s -> Step a s) -> Up s -> Step a s
    find f step s stop yield = [||
       let go s = $$(step [||s||] stop (\a s ->
                       [|| if $$(f a) then $$(yield a s) else go $$s ||]))
       in go $$s ||]

This iterates until the stream ends or we find a value for which the predicate
holds.

    filter :: (Up a -> Up Bool) -> Pull a -> Pull a
    filter f (Pull seed step) = Pull seed (find f step)

This definition is generally efficient in practice, and we don't lose any
"fusion". The only disadvantage is that iterated filtering produces larger than
optimal code, because each `filter` outputs a separate loop for skipping. I
don't think it's a big issue, because iterated filtering is not a common
pattern, and the performance penalty is not too bad in any case.

Moving to `zipWith`. The idea is to pair up the internal states of two streams
and step them in lockstep. For this, it makes sense to use strict pairs and some
helpers.

    data Pair a b = Pair !a !b

    pair :: Up a -> Up b -> Up (Pair a b)
    pair a b = [|| Pair $$a $$b ||]

    bindPair :: Up (Pair a b) -> (Up a -> Up b -> Up c) -> Up c
    bindPair ab f = [|| case $$ab of Pair a b -> $$(f [||a||] [||b||]) ||]

I also enable `BlockArguments` to get rid of some parens.

    {-# language BlockArguments #-}

    zipWith :: (Up a -> Up b -> Up c) -> Pull a -> Pull b -> Pull c
    zipWith f (Pull seed step) (Pull seed' step') =
      Pull (pair seed seed') \s stop yield ->
        bindPair s \s s' ->
        step s stop \a s ->
        step' s' stop \b s' ->
        yield (f a b) (pair s s')

In short, `zipWith` yields if both streams yield, otherwise stops. Here we
benefit from not having `Skip`, because with `Skip` we'd need to handle nine
cases instead of four.

### No binding and appending for `Pull`

In this repo we don't have the following operations:

    (<>)  :: Pull a -> Pull a -> Pull a
	(>>=) :: Pull a -> (a -> Pull b) -> Pull b

Users should instead convert from `Pull` to `Push`, and then bind or append
there. What's the reason for this?

I won't talk in detail about binding; the main issue is that it requires
sigma-types in the object language, and Haskell doesn't have sigma-types.

For appending, the issue is that it requires `-O2` for adequate compilation in
Haskell, because it needs [call pattern specialization]([call pattern
specialization](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.145.7980&rep=rep1&type=pdf)).

In general, we want to avoid introducing sum types in `Pull` states, and
appending requires sum types (and binding too). Let's look at the definition for
appending:

    (<>) :: Pull a -> Pull a -> Pull a
    (<>) (Pull seed step) (Pull seed' step') =
      Pull [||Left $$seed||] \s stop yield -> [||
        case $$s of
          Left s  -> $$(step  [||s||]
                              (step' seed' stop \a s -> yield a [||Right $$s||])
                              \a s -> yield a [||Left $$s||])
          Right s -> $$(step' [||s||]
                              stop
                              \a s -> yield a [||Right $$s||])
        ||]

The way this works is that the internal state of `xs <> ys` is the sum of the
states of `xs` and `ys`, and a `Left` state marks that we're processing
currently `xs`, and `Right` marks that we've finished consuming `xs` and we're
processing `ys`. The stepper function checks which kind of state we're in and
dispatches accordingly.

If we write a function which iterates this stepper function, we don't want the
code output to actually contain pattern matching on `Left` and `Right`. That
kills any unboxing that could happen in the sub-states, and it has some overhead
on its own. Instead we want to split the iteration into *two* recursive
functions. The first function consumes `xs` and calls the second function when
it finishes. The second function consumes `ys`.

Call pattern specialization can do this kind of transformation. But as I
mentioned it's an `-O2` optimization. We can turn it on independently of `-O2`,
but I think that's a fine-print detail that should not be imposed on library
users, preferably. Library users overwhelmingly use plain `-O1` so fusion
libraries should work robustly with plain `-O1`.


## The code generation monad

Let's look at `drop` for `Pull`. Here be bump into a GHC-specific issue.  Let's
do a first attempt:

    dropState :: Up Int -> (Up s -> Step a s) -> Up s -> Up s
    dropState n step s = [||
      let go n s | n <= (0::Int) = seq s s
                 | otherwise     = $$(step [||s||] [||s||] (\_ s -> [||go (n - 1) $$s||]))
      in go $$n $$s ||]

    drop :: Up Int -> Pull a -> Pull a
    drop n (Pull seed step) = Pull (dropState n step seed) step

This is a bit similar to filtering. We modify the starting state, by rolling it
forward by `n` steps. Why is this definition problematic? The reason is GHC's
inability to perform nested unboxing in return types, as of GHC-9.2.3, although
GHC-9.4 [does promise this
feature](https://discourse.haskell.org/t/ghc-9-4-1-rc1-is-now-available/4817). If
the stream state is a tuple, which can happen if it's a zipped stream, then the
result of the `go` function will be boxed and then probably immediately unboxed.

This isn't a huge issue, but we can fix it and the fix is interesting and
more generally applicable, so let's look at it.

The fix is to introduce a monad whose side effect is outputting code. This
provides more control over code generation, and in particular we can specify
*where* to continue inserting code. The solution for `dropState` will be that we
insert generated code *inside* the base case of tail-recursive `go` definition.
Instead of returning `seq s s` there, we will have `seq s` followed by the rest
of whatever code we generate. This means that `s` doesn't appear as the output
type of a recursive function.

We call the generation monad `Gen`:

    newtype Gen a = Gen {unGen :: forall r. (a -> Up r) -> Up r}

This is just the CPS'd identity monad + the restriction that we must return in `Up`.

    instance Functor Gen where
      fmap f ma = Gen \k -> unGen ma \a -> k (f a)

    instance Applicative Gen where
      pure a = Gen \k -> k a
      (<*>) gf ga = Gen \k -> unGen gf \f -> unGen ga \a -> k (f a)

    instance Monad Gen where
      return = pure
      (>>=) ga f = Gen \k -> unGen ga \a -> unGen (f a) k

We can only run `Gen (Up a)`:

    run :: Gen (Up a) -> Up a
    run ma = unGen ma id

*let insertion* is a useful feature which is available in `Gen`. It lets us
insert an object-level binder anywhere:

    ilet :: Up a -> (Up a -> Gen b) -> Gen b
    ilet a f = Gen \k -> [|| let x = $$a in $$(unGen (f [||x||]) k) ||]

    ilet' :: Up a -> (Up a -> Gen b) -> Gen b
    ilet' a f = Gen \k -> [|| let x = $$a in seq x $$(unGen (f [||x||]) k) ||]

If we sequence a bunch of `Gen` actions together, each of them may have
the effect of generating some code, and they are performed in order when
we `run` the action.

For example, we might want to take a meta-level list of expressions, let-bind
all expressions to a variable, and return the list of variables:

    bindAll :: [Up a] -> Gen [Up a]
	bindAll = mapM (\a -> ilet a pure)

While we're working inside `Gen`, we don't have to explicitly manipulate scopes
or generate fresh variables, and we have implicit well-typed access to newly
inserted bound variables.

## Push and Pull in the Gen monad

In the actual code here in the repository, I modify the definitions of `Push`
and `Pull` as follows:

    data Push' a = Push' {
      len  :: Maybe (Up Int),
      fold :: forall l. (Up a -> Up l -> Up l) -> Up l -> Up l}

    type Push a = Gen (Push' a)

    type Step a s = forall r. Up r -> (Up a -> Up s -> Up r) -> Up r

    data Pull' a =
	  forall s. Pull' {len :: Maybe (Up Int), seed :: Up s, step :: Up s -> Step a s}

    type Pull a = Gen (Pull' a)

There are two main differences. First, now both `Push` and `Pull` are wrapped in
`Gen`. This makes it possible to fix `drop` for `Pull` in the following way:

    dropState :: Up Int -> (Up s -> Step a s) -> Up s -> Gen (Up s)
    dropState n step s = Gen \ret -> [||
      let go n s | n <= (0::Int) = seq s $$(ret [||s||])
                 | otherwise     = $$(step [||s||] (ret [||s||]) (\_ s -> [||go (n - 1) $$s||]))
      in go $$n $$s ||]

    drop :: Up Int -> Pull a -> Pull a
    drop n pa = do
      Pull' len seed step <- pa
      seed <- dropState n step seed
      pure $ Pull' (U.max 0 <$> ((-) <$> len <*> Just n)) seed step

Notice that in `dropState`, the usage of `ret` marks where we continue inserting
generated code. I suggestively name the continuation `ret`, because it works like a
CPS'd return address.

Second, I extend `Push` and `Pull` with sizing information. `Nothing` marks that
I don't have static information about sizes. The primary use case is that if we
know the expression which computes the size of a stream, we can convert them to
arrays more efficiently because we can allocate the target buffer
beforehand. This makes it possible to compile mapping and zipping on arrays to
near-optimal code.

Sometimes I want to force the computation of array sizes just once, instead of
copying the size-computing expression to multiple places. So I use strict
let-insertion to force computations. Hence the usage of the `Gen` monad
in `Push` as well.

It would be feasible to make size computations a lot smarter and also to
add a lot more annotation to both `Push` and `Pull`.

## Asymmetric zipping

Although we can't zip `Push`, we can zip a `Push` and a `Pull`, and what we get
is a `Pull`. You can look at `zipWithPull` in [`Push.hs`](Push.hs). The idea is
that we fold over the `Push` with an accumulator which stores the state of the
`Pull` stream. At each iteration we can process the next `Push` value and also
progress the `Pull` machine.

This is an important feature, because *many* practical programs are expressible
with only asymmetric zipping.
