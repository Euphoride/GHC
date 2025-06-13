module GHC.Core.Opt.RemoveMatchable where

import Data.Maybe (mapMaybe)
import GHC.Builtin.Names (matchableClassName)
import GHC.Core
import GHC.Core (CoreRule (..))
import GHC.Core.Class (Class (..), ClassBody (..), className)
import GHC.Core.Coercion (Role (..), coercionKind, mkUnivCo)
import GHC.Core.DataCon (DataCon (..), dataConTyCon)
import GHC.Core.Opt.Arity (typeArity)
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Rep (Coercion (..), KindCoercion, MCoercion (..), PredType, Scaled (..), ThetaType, Type (..), UnivCoProvenance (..), mkInvisForAllTys)
import GHC.Core.TyCon (AlgTyConFlav (..), AlgTyConRhs (..), TyCon (..), TyConDetails (..), tyConClass_maybe)
import GHC.Core.Type (Scaled (..), splitCastTy_maybe, splitTyConApp_maybe)
import GHC.Data.Pair
import GHC.Prelude
import GHC.Tc.Utils.TcType (TcInvisTVBinder, mkPhiTy, tcSplitForAllInvisTVBinders, tcSplitPhiTy, tcSplitPredFunTy_maybe)
import GHC.Types.Basic
import GHC.Types.Id (idArity, idSpecialisation, idType, idUnfolding, isDataConId_maybe, isId, setIdArity, setIdSpecialisation, setIdType, setIdUnfolding, setInlinePragma, Id)
import GHC.Types.Id.Info
import GHC.Types.Var (Var (..), setVarType)
import GHC.Unit.Module.ModGuts
import GHC.Utils.Outputable

removeMatchableThetas :: Type -> (Type, Int)
transformExpr :: CoreExpr -> CoreExpr
scrubId :: Id -> Id
