
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module defines TyCons that can't be expressed in Haskell.
--   They are all, therefore, wired-in TyCons.  C.f module "GHC.Builtin.Types"
module GHC.Builtin.Types.Prim where
import GHC.Types.Var (FunTyFlag)
import GHC.Core.TyCon (TyCon)

funTyFlagTyCon :: FunTyFlag -> TyCon
