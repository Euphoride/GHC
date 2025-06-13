{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Tc.Solver.Monad ( emitNewGivens, runTcS )
where

import GHC.Prelude

import GHC.Driver.Env

import qualified GHC.Tc.Utils.Instantiate as TcM
import GHC.Core.InstEnv
import GHC.Tc.Instance.Family as FamInst
import GHC.Core.FamInstEnv

import qualified GHC.Tc.Utils.Monad    as TcM
import qualified GHC.Tc.Utils.TcMType  as TcM
-- import qualified GHC.Tc.Instance.Class as TcM( matchGlobalInst, ClsInstResult(..) )
import qualified GHC.Tc.Utils.Env      as TcM
       ( checkWellStaged, tcGetDefaultTys
       , tcLookupClass, tcLookupId, tcLookupTyCon
       , topIdLvl )
import GHC.Tc.Zonk.Monad ( ZonkM )
import qualified GHC.Tc.Zonk.TcType  as TcM
import qualified GHC.Tc.Zonk.Type as TcM

import GHC.Driver.DynFlags

-- import GHC.Tc.Instance.Class( safeOverlap, instanceReturnsDictCon )
import GHC.Tc.Instance.FunDeps( FunDepEqn(..) )


import GHC.Tc.Solver.Types
import GHC.Tc.Solver.InertSet
import GHC.Tc.Errors.Types

import GHC.Tc.Utils.TcType
-- import GHC.Tc.Utils.Unify

import GHC.Tc.Types.Evidence
import GHC.Tc.Types
import GHC.Tc.Types.Origin
import GHC.Tc.Types.CtLoc
import GHC.Tc.Types.Constraint

import GHC.Builtin.Names ( unsatisfiableClassNameKey, callStackTyConName, exceptionContextTyConName )

import GHC.Core.Type
import GHC.Core.TyCo.Rep as Rep
import GHC.Core.TyCo.Tidy
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom( TypeEqn )
import GHC.Core.Predicate
import GHC.Core.Reduction
import GHC.Core.Class
import GHC.Core.TyCon
import GHC.Core.Unify (typesAreApart)

import GHC.Types.Name
import GHC.Types.TyThing
import GHC.Types.Name.Reader
import GHC.Types.DefaultEnv ( DefaultEnv )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Unique.Supply
import GHC.Types.Unique.Set( elementOfUniqSet )

import GHC.Unit.Module ( HasModule, getModule, extractModule )
import qualified GHC.Rename.Env as TcM

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Utils.Misc (HasDebugCallStack, (<||>))

import GHC.Data.Bag as Bag
import GHC.Data.Pair

import GHC.Utils.Monad

import GHC.Exts (oneShot)
import Control.Monad
import Data.Foldable hiding ( foldr1 )
import Data.IORef
import Data.List ( mapAccumL )
import Data.List.NonEmpty ( nonEmpty )
import qualified Data.List.NonEmpty as NE
import Data.Maybe ( isJust )
import qualified Data.Semigroup as S
import GHC.Types.SrcLoc
import GHC.Rename.Env

#if defined(DEBUG)
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Data.Graph.Directed
#endif


-- | See Note [TcSMode]
data TcSMode
  = TcSVanilla    -- ^ Normal constraint solving
  | TcSEarlyAbort -- ^ Abort early on insoluble constraints
  | TcSSpecPrag -- ^ Fully solve all constraints
--   deriving (Eq)

{- Note [TcSMode]
~~~~~~~~~~~~~~~~~
The constraint solver can operate in different modes:

* TcSVanilla: Normal constraint solving mode. This is the default.

* TcSEarlyAbort: Abort (fail in the monad) as soon as we come across an
  insoluble constraint. This is used to fail-fast when checking for hole-fits.
  See Note [Speeding up valid hole-fits].

* TcSSpecPrag: Solve constraints fully or not at all. This is described in
  Note [TcSSpecPrag].

  This mode is currently used in one place only: when solving constraints
  arising from specialise pragmas.
  See Note [Fully solving constraints for specialisation] in GHC.Tc.Gen.Sig.
-}

data TcSEnv
  = TcSEnv {
      tcs_ev_binds    :: EvBindsVar,

      tcs_unified     :: IORef Int,
         -- The number of unification variables we have filled
         -- The important thing is whether it is non-zero, so it
         -- could equally well be a Bool instead of an Int.

      tcs_unif_lvl  :: IORef (Maybe TcLevel),
         -- The Unification Level Flag
         -- Outermost level at which we have unified a meta tyvar
         -- Starts at Nothing, then (Just i), then (Just j) where j<i
         -- See Note [The Unification Level Flag]

      tcs_count     :: IORef Int, -- Global step count

      tcs_inerts    :: IORef InertSet, -- Current inert set

      -- | The mode of operation for the constraint solver.
      -- See Note [TcSMode]
      tcs_mode :: TcSMode,

      tcs_worklist :: IORef WorkList
    }

---------------
newtype TcS a = TcS { unTcS :: TcSEnv -> TcM a }
--   deriving (Functor)

instance MonadFix TcS

-- | Smart constructor for 'TcS', as describe in Note [The one-shot state
-- monad trick] in "GHC.Utils.Monad".
mkTcS :: (TcSEnv -> TcM a) -> TcS a
-- mkTcS f = TcS (oneShot f)

instance Applicative TcS

instance Monad TcS

instance MonadIO TcS

instance MonadFail TcS 

instance MonadUnique TcS

instance HasModule TcS 

instance MonadThings TcS 

emitNewGivens :: CtLoc -> [(Role,TcCoercion)] -> TcS ()
runTcS :: TcS a                -- What to run
       -> TcM (a, EvBindMap)