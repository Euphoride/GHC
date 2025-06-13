{-# LANGUAGE MultiWayIf #-}

module GHC.Tc.Instance.Class (annotateTyCon) where
import GHC.Core.TyCon
import GHC.Tc.Types (TcM)

annotateTyCon :: TyCon -> TcM ()