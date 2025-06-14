{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Magic
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-internal/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC magic.
--
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Internal.Magic ( inline, noinline, lazy, oneShot, runRW#, DataToTag(..) ) where

--------------------------------------------------
--        See Note [magicIds] in GHC.Types.Id.Make
--------------------------------------------------

-- Here import TYPE explicitly from GHC.Internal.Types and not from GHC.Internal.Prim. This is
-- because TYPE is not exported by the source Haskell module generated by
-- genprimops which Haddock will typecheck (#15935).
import GHC.Internal.Prim (State#, realWorld#, RealWorld, Int#)
import GHC.Internal.Types (RuntimeRep(BoxedRep), TYPE, Levity, Constraint)

-- | The call @inline f@ arranges that @f@ is inlined, regardless of
-- its size. More precisely, the call @inline f@ rewrites to the
-- right-hand side of @f@'s definition. This allows the programmer to
-- control inlining from a particular call site rather than the
-- definition site of the function (c.f. @INLINE@ pragmas).
--
-- This inlining occurs regardless of the argument to the call or the
-- size of @f@'s definition; it is unconditional. The main caveat is
-- that @f@'s definition must be visible to the compiler; it is
-- therefore recommended to mark the function with an @INLINABLE@
-- pragma at its definition so that GHC guarantees to record its
-- unfolding regardless of size.
--
-- If no inlining takes place, the 'inline' function expands to the
-- identity function in Phase zero, so its use imposes no overhead.
{-# NOINLINE[0] inline #-}
inline :: a -> a
inline x = x

-- | The call @noinline f@ arranges that @f@ will not be inlined.
-- It is removed during CorePrep so that its use imposes no overhead
-- (besides the fact that it blocks inlining.)
noinline :: a -> a
{-# NOINLINE noinline #-}  -- noinline is inlined manually in CorePrep
noinline x = x

-- | The 'lazy' function restrains strictness analysis a little. The
-- call @lazy e@ means the same as @e@, but 'lazy' has a magical
-- property so far as strictness analysis is concerned: it is lazy in
-- its first argument, even though its semantics is strict. After
-- strictness analysis has run, calls to 'lazy' are inlined to be the
-- identity function.
--
-- This behaviour is occasionally useful when controlling evaluation
-- order. Notably, 'lazy' is used in the library definition of
-- 'Control.Parallel.par':
--
-- > par :: a -> b -> b
-- > par x y = case (par# x) of _ -> lazy y
--
-- If 'lazy' were not lazy, 'Control.Parallel.par' would look strict in
-- @y@ which would defeat the whole purpose of 'Control.Parallel.par'.
lazy :: a -> a
{-# NOINLINE lazy #-}  -- lazy is inlined manually in CorePrep
lazy x = x
-- Implementation note: its strictness and unfolding are over-ridden
-- by the definition in GHC.Types.Id.Make; in both cases to nothing at all.
-- That way, 'lazy' does not get inlined, and the strictness analyser
-- sees it as lazy.  Then the worker/wrapper phase inlines it.
-- Result: happiness


-- | The 'oneShot' function can be used to give a hint to the compiler that its
-- argument will be called at most once, which may (or may not) enable certain
-- optimizations. It can be useful to improve the performance of code in continuation
-- passing style.
--
-- If 'oneShot' is used wrongly, then it may be that computations whose result
-- that would otherwise be shared are re-evaluated every time they are used. Otherwise,
-- the use of `oneShot` is safe.
--
-- 'oneShot' is representation-polymorphic: the type variables may refer to lifted
-- or unlifted types.
oneShot :: forall (q :: RuntimeRep) (r :: RuntimeRep)
                  (a :: TYPE q) (b :: TYPE r).
           (a -> b) -> a -> b
oneShot f = f
-- Implementation note: This is wired in in GHC.Types.Id.Make, so the code here is
-- mostly there to have a place for the documentation.

-- | Apply a function to a @'State#' 'RealWorld'@ token. When manually applying
-- a function to `realWorld#`, it is necessary to use @NOINLINE@ to prevent
-- semantically undesirable floating. `runRW#` is inlined, but only very late
-- in compilation after all floating is complete.

-- 'runRW#' is levity-polymorphic: the result may have a lifted or
-- unlifted type.

runRW# :: forall (r :: RuntimeRep) (o :: TYPE r).
          (State# RealWorld -> o) -> o
-- See Note [runRW magic] in GHC.CoreToStg.Prep.
{-# NOINLINE runRW# #-}  -- runRW# is inlined manually in CorePrep
runRW# m = m realWorld#

-- | @'dataToTag#'@ evaluates its argument and returns the index
-- (starting at zero) of the constructor used to produce that
-- argument.  Any algebraic data type with all of its constructors
-- in scope may be used with @dataToTag#@.
--
-- >>> dataToTag# (Left ())
-- 0#
-- >>> dataToTag# (Right undefined)
-- 1#
type DataToTag :: forall {lev :: Levity}. TYPE (BoxedRep lev) -> Constraint
-- See Note [DataToTag overview] in GHC.Tc.Instance.Class.
--
-- Ignoring DatatypeContexts, any generated DataToTag instance is
-- equivalent to a function the user could have written themselves.
-- So it does not get its own Unsafe module, unlike WithDict.
class DataToTag a where
  dataToTag# :: a -> Int#
