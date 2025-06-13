{-# LANGUAGE CPP #-}

module GHC.Core.Opt.RemoveMatchable where

-- #define DEBUG_MOD

import Data.Maybe (mapMaybe)
import GHC.Builtin.Names (matchableClassName)
import GHC.Core
import GHC.Core (CoreRule (..))
import GHC.Core.Class (Class (..), ClassBody (..), className)
import GHC.Core.Coercion (Role (..), coercionKind, mkUnivCo)
import GHC.Core.DataCon (DataCon (..), dataConTyCon, DataConRep(..), dataConName, dataConRepStrictness, dataConRepArgTys)
import GHC.Core.Opt.Arity (typeArity)
import GHC.Core.Opt.Monad
import GHC.Core.Predicate (getClassPredTys_maybe)
import GHC.Core.TyCo.Rep (Coercion (..), KindCoercion, MCoercion (..), PredType, Scaled (..), ThetaType, Type (..), UnivCoProvenance (..), mkInvisForAllTys)
import GHC.Core.TyCon (AlgTyConFlav (..), AlgTyConRhs (..), TyCon (..), TyConDetails (..), tyConClass_maybe, TyConRepName)
import GHC.Core.Type (Scaled (..), splitCastTy_maybe, splitTyConApp_maybe)
import GHC.Data.Pair
import GHC.Prelude
import GHC.Tc.Utils.TcType (TcInvisTVBinder, mkPhiTy, tcSplitForAllInvisTVBinders, tcSplitPhiTy, tcSplitPredFunTy_maybe)
import GHC.Types.Basic
import GHC.Types.Id (idArity, idSpecialisation, idType, idUnfolding, isDataConId_maybe, isId, setIdArity, setIdSpecialisation, setIdType, setIdUnfolding, setInlinePragma)
import GHC.Types.Id.Info (IdDetails(..), RuleInfo(..))
import GHC.Types.Id.Make (DataConBoxer(..))
import GHC.Types.Var (Var (..), CoVar(..), setVarType, Id, idDetails)
import GHC.Unit.Module.ModGuts
import GHC.Utils.Outputable

import GHC.Core.Unfold.Make (mkCoreUnfolding)

import Control.Arrow (second)


-- =====================================
-- Predicates
-- =====================================


isMatchableType :: Type -> Bool
isMatchableType ty =
  case splitTyConApp_maybe ty of
    Just (tc, _) ->
      case tyConClass_maybe tc of
        Just cls -> className cls == matchableClassName
        Nothing -> False
    Nothing -> False

isScaledMatchableType :: Scaled Type -> Bool
isScaledMatchableType (Scaled _ ty) = isMatchableType ty

isMatchableVar :: Var -> Bool
isMatchableVar v = isId v && isMatchableType (varType v)

isMatchableBinder :: Var -> Bool
isMatchableBinder = isMatchableVar

isMatchableDataCon :: OutExpr -> Bool
isMatchableDataCon (Var v) =
  case isDataConId_maybe v of
    Just dc ->
      case tyConClass_maybe (dataConTyCon dc) of
        Just cls -> className cls == matchableClassName
        Nothing -> False
    Nothing -> False
isMatchableDataCon _ = False


isMatchableTheta :: PredType -> Bool
isMatchableTheta theta = case getClassPredTys_maybe theta of
  Nothing -> False
  Just (cls, _) -> className cls == matchableClassName


isMatchableCoercion :: Coercion -> Bool
isMatchableCoercion co =
  case coercionKind co of
    Pair lty rty -> isMatchableType lty || isMatchableType rty


isMatchableDataConApp :: CoreExpr -> CoreExpr -> Bool
isMatchableDataConApp (App h _) _ = isMatchableDataCon h
isMatchableDataConApp _ _ = False

isMatchableArg :: CoreExpr -> Bool
isMatchableArg (Var v) = isMatchableVar v
isMatchableArg (Cast e co) = isMatchableArg e
isMatchableArg _ = False



-- =====================================
-- Type manipulation
-- =====================================



unwrapAllCasts :: Type -> [KindCoercion] -> (Type, [KindCoercion])
unwrapAllCasts ty cos = case splitCastTy_maybe ty of
  Just (inner_ty, co) -> unwrapAllCasts inner_ty (co : cos)
  Nothing -> (ty, cos)

wrapCasts :: Type -> [KindCoercion] -> Type
wrapCasts ty [] = ty
wrapCasts ty (co : cos) = wrapCasts (CastTy ty co) cos

decomp :: Type -> (([TcInvisTVBinder], ThetaType, Type), [KindCoercion])
decomp ty =
  let (bndrs, body) = tcSplitForAllInvisTVBinders ty
      (body', cos) = unwrapAllCasts body []
      theta = collectAllThetas body'
      tau = dropAllThetas body'
      tau' = walkType tau
   in ((bndrs, theta, tau'), cos)
  where
    collectAllThetas ty =
      case tcSplitPredFunTy_maybe ty of
        Just (pred, rest) -> pred : collectAllThetas rest
        Nothing -> []

    dropAllThetas ty =
      case tcSplitPredFunTy_maybe ty of
        Just (_, rest) -> dropAllThetas rest
        Nothing -> ty

    walkType :: Type -> Type
    walkType ty = case ty of
      ForAllTy {} ->
        let (processed, _) = removeMatchableThetas ty
         in processed
      FunTy af m m' arg res ->
        FunTy af m m' (walkType arg) (walkType res)
      AppTy fun arg ->
        AppTy (walkType fun) (walkType arg)
      TyConApp tc args ->
        TyConApp tc (map walkType args)
      CastTy inner co ->
        CastTy (walkType inner) co
      _ -> ty

removalPass :: ([TcInvisTVBinder], ThetaType, Type) -> ([TcInvisTVBinder], ThetaType, Type)
removalPass (bndrs, theta, ty) = (bndrs, theta', ty)
  where
    theta' = filter (not . isMatchableTheta) theta


collect :: (([TcInvisTVBinder], ThetaType, Type), [KindCoercion]) -> Type
collect ((bndrs, theta, tau), cos) = final_ty
  where
    phi_ty = mkPhiTy theta tau
    casted = wrapCasts phi_ty cos
    final_ty = mkInvisForAllTys bndrs casted

removeMatchableThetas :: Type -> (Type, Int)
removeMatchableThetas ty = (ty', length theta - length theta')
  where
    (inner@(_, theta, _), cos) = decomp ty
    inner'@(_, theta', _) = removalPass inner
    ty' = collect (inner', cos)


-- =====================================
-- ID/Var updates
-- =====================================


updateIdType :: Var -> Var
updateIdType v =
  let (ty, removed_args) = removeMatchableThetas (varType v)
      v' = v `setVarType` ty
   in if isId v then v' `setIdArity` (typeArity ty) else v'

   
scrubBinder :: Var -> Var
scrubBinder b =
  let b1 = (scrubId b) `setIdUnfolding` NoUnfolding `setInlinePragma` neverInlinePragma
      (RuleInfo spec vars) = idSpecialisation b1
      spec' = mapMaybe nukeMatchableFromRule spec
   in b1 `setIdSpecialisation` (RuleInfo spec' vars)


updateCoVarType :: CoVar -> CoVar
updateCoVarType cv = 
  setVarType cv (fst (removeMatchableThetas (varType cv)))


-- =====================================
-- DataCon updates
-- =====================================

filterByIndices :: [Int] -> [a] -> [a]
filterByIndices indices xs = 
  [x | (i, x) <- zip [0..] xs, i `elem` indices] 


updateDataCon :: DataCon -> DataCon
updateDataCon dc = 
  let
      old_theta = dcOtherTheta dc
      new_theta = filter (not . isMatchableTheta) old_theta
      removed_theta_count = length old_theta - length new_theta
      
      (new_rep_type, removed_type_count) = removeMatchableThetas (dcRepType dc)
      
      new_rep_arity = dcRepArity dc - removed_theta_count
      
      -- indices_to_keep = [i | (i, arg) <- zip [0..] (dcOrigArgTys dc), 
      --                       not (isScaledMatchableType arg)]
      
      
      (new_rep, indices) = updateDataConRep (dcRep dc)
      new_stricts = filterByIndices indices (dcStricts dc)

  in pprTrace "=== UPDATING DATACON ===" 
       (text "DataCon:" <+> ppr (dataConName dc) $$
        text "Old theta count:" <+> int (length old_theta) $$
        text "New theta count:" <+> int (length new_theta) $$
        text "Removed theta count:" <+> int removed_theta_count $$
        text "Old rep arity:" <+> int (dcRepArity dc) $$
        text "New rep arity:" <+> int new_rep_arity $$
        text "Type args removed:" <+> int removed_type_count $$
        text "Old rep type:" <+> ppr (dcRepType dc) $$
        text "New rep type:" <+> ppr new_rep_type $$
        text "dcRepArity:" <+> int (dcRepArity dc) $$
        text "length (dataConRepArgTys dc):" <+> int (length (dataConRepArgTys dc)) $$
        text "length dcStricts:" <+> int (length (dcStricts dc)) $$
        text "length (dataConRepStrictness dc):" <+> int (length (dataConRepStrictness dc))) $
     dc { dcOtherTheta = new_theta
        , dcRepType = new_rep_type
        , dcWorkId = scrubId (dcWorkId dc)
        , dcRep = new_rep
        , dcRepArity = new_rep_arity
        , dcStricts = new_stricts
        }

updateDataConRep :: DataConRep -> (DataConRep, [Int])
updateDataConRep NoDataConRep = (NoDataConRep, [])
updateDataConRep (DCR wrap_id (DCB boxer) arg_tys) = 
  (DCR { dcr_wrap_id = updateIdForWrapper wrap_id
      -- , dcr_boxer = DCB $ \ty_args src_vars -> do
      --     (x, y) <- boxer ty_args src_vars
      --     return (filterByIndices indices_to_keep x, filterByIndices indices_to_keep y)
      , dcr_boxer = DCB boxer
      , dcr_arg_tys = new_args
      }, indices)
  where 
    indexed_args = zip [0..] arg_tys
    new_indexed_args = filter (not . isScaledMatchableType . snd) indexed_args
    (indices, new_args) = unzip new_indexed_args

updateIdForWrapper :: Id -> Id
updateIdForWrapper id = 
  let id' = scrubId id
      id'' = case idUnfolding id of
               CoreUnfolding { uf_tmpl = expr, uf_src = src, uf_is_top = top
                             , uf_cache = cache, uf_guidance = guide } ->
                 let expr' = transformExpr expr
                     unf' = mkCoreUnfolding src top expr' Nothing guide
                 in setIdUnfolding id' unf'
               
               DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = args } ->
                 let bndrs' = map scrubId bndrs
                     args' = map transformExpr args
                     unf' = DFunUnfolding { df_bndrs = bndrs'
                                          , df_con = con
                                          , df_args = args' }
                 in setIdUnfolding id' unf'
               
               _ -> id'
  in id''


scrubId :: Id -> Id
scrubId id@(Id {}) = 
  let id' = updateIdType id
      details' = case idDetails id' of
                   DataConWorkId dc -> DataConWorkId (updateDataCon dc)
                   other -> other
      result = id' { id_details = details' }
  in  result
scrubId otherwise = 
  let result = updateIdType otherwise
  in result


-- =====================================
-- TyCon updates
-- =====================================


transformTyCon :: TyCon -> TyCon
transformTyCon tc = 
  case tyConDetails tc of
    AlgTyCon { algTcFlavour = ClassTyCon clas parents
             , algTcRhs = rhs } ->
      tc { tyConDetails = updateAlgTyCon (tyConDetails tc) clas parents rhs }
    AlgTyCon { algTcRhs = dts@(DataTyCon { data_cons = dcs }) } ->
      let dcs' = map updateDataCon dcs
          details' = (tyConDetails tc) { algTcRhs = dts {data_cons = dcs'} }
      in tc { tyConDetails = details' }
    _ -> tc




updateAlgTyCon :: TyConDetails -> Class -> TyConRepName -> AlgTyConRhs -> TyConDetails
updateAlgTyCon details clas parents rhs =
  case (classBody clas, rhs) of
    (stuff@(ConcreteClass {}), dts@(DataTyCon {data_cons = dcs})) ->
      let 
          ops' = [(scrubId op_sel, defn) | (op_sel, defn) <- cls_ops stuff]
          stuff' = stuff { cls_ops = ops' }
          clas' = clas { classBody = stuff' }
          
          dcs' = map updateDataCon dcs
          
      in details { algTcFlavour = ClassTyCon clas' parents
                 , algTcRhs = dts { data_cons = dcs' } }
    _ -> details


updateScaledType :: Scaled Type -> Scaled Type
updateScaledType (Scaled m ty) = Scaled m (fst (removeMatchableThetas ty))

-- =====================================
-- Core transformation
-- =====================================


transformBind :: CoreBind -> CoreBind
transformBind (NonRec b e) = NonRec (scrubBinder b) (transformExpr e)
transformBind (Rec pairs) = Rec [(scrubBinder b, transformExpr e) | (b, e) <- pairs]




transformExpr :: CoreExpr -> CoreExpr
transformExpr = go
  where
    go :: CoreExpr -> CoreExpr
    go (Var v) = Var (scrubId v)
    go (Lit l) = Lit l
    go (Type t) = Type t
    go (Coercion co) = Coercion (transformCoercion co)
    go (App f (App (App dict arg) arg')) | isMatchableDataCon dict = go f
    go (App f (Cast (App (App dict arg) arg') _)) | isMatchableDataCon dict = go f
    go (App f a) = transformApp f a
    go (Lam b e) = transformLam b e
    go (Let bind e) = transformLet bind e
    go (Case e b ty alts) = 
      Case (go e) 
           (scrubId b) 
           (fst (removeMatchableThetas ty)) 
           (map transformAlt alts)
    go (Cast e co) = transformCast e co
    go (Tick t e) = Tick t (go e)

    transformApp :: CoreExpr -> CoreExpr -> CoreExpr
    transformApp f a
      | isMatchableArg a = go f  
      | isMatchableDataConApp f a = go (getDataConHead f) 
      | otherwise = App (go f) (go a)


    getDataConHead :: CoreExpr -> CoreExpr
    getDataConHead (App f _) = getDataConHead f
    getDataConHead e = e

    transformLam :: Var -> CoreExpr -> CoreExpr
    transformLam b e
      | isMatchableBinder b = go e  
      | otherwise = Lam (scrubId b) (go e)

    transformLet :: CoreBind -> CoreExpr -> CoreExpr
    transformLet (NonRec b _) e | isMatchableBinder b = go e 
    transformLet bind e = Let (transformBind bind) (go e)

    transformCast :: CoreExpr -> Coercion -> CoreExpr
    transformCast e co =
      let e' = go e
          Pair from_ty to_ty = coercionKind co
          (from_ty', _) = removeMatchableThetas from_ty
          (to_ty', _) = removeMatchableThetas to_ty
      in Cast e' (mkUnivCo (PluginProv "erase-dict") [] Representational from_ty' to_ty')

transformCoercion :: Coercion -> Coercion
transformCoercion = go
 where
   go :: Coercion -> Coercion
   go (Refl ty) = 
     Refl (fst (removeMatchableThetas ty))
   
   go (GRefl r ty mco) = 
     GRefl r (fst (removeMatchableThetas ty)) (transformMCo mco)
   
   go (TyConAppCo r tc cos) = 
     TyConAppCo r tc (map go cos)
   
   go (AppCo co1 co2) = 
     AppCo (go co1) (go co2)
   
   go (ForAllCo tcv visL visR kco body) =
     ForAllCo (updateCoVarType tcv) visL visR (go kco) (go body)
   
   go (FunCo r afl afr mult mat arg res)
     | isMatchableCoercion arg = go res 
     | otherwise = FunCo r afl afr (go mult) (go mat) (go arg) (go res)
   
   go (CoVarCo cv) = 
     CoVarCo (updateCoVarType cv)
   
   go (AxiomCo ax cos) = 
     AxiomCo ax (map go cos)
   
   go (UnivCo prov r lty rty deps) =
     UnivCo prov r 
       (fst (removeMatchableThetas lty)) 
       (fst (removeMatchableThetas rty)) 
       (map go deps)
   
   go (SymCo co) = SymCo (go co)
   go (TransCo co1 co2) = TransCo (go co1) (go co2)
   go (SelCo sel co) = SelCo sel (go co)
   go (LRCo lr co) = LRCo lr (go co)
   go (InstCo co1 co2) = InstCo (go co1) (go co2)
   go (KindCo co) = KindCo (go co)
   go (SubCo co) = SubCo (go co)
   go (HoleCo h) = HoleCo h

transformMCo :: MCoercion -> MCoercion
transformMCo MRefl = MRefl
transformMCo (MCo co) = MCo (transformCoercion co)



transformAlt :: CoreAlt -> CoreAlt
transformAlt (Alt con bs e) = Alt (transformAltCon con) (((map scrubId) . (filter (not . isMatchableVar))) bs) (transformExpr e)

transformAltCon :: AltCon -> AltCon
transformAltCon (DataAlt datacon) = DataAlt $ updateDataCon datacon
transformAltCon otherwise = otherwise


-- =====================================
-- Module passes
-- =====================================


removeMatchablePass :: CoreProgram -> CoreM CoreProgram
removeMatchablePass binds = do
  let binds' = map transformBind binds
  pprTrace "*** INPUT CORE PROGRAM ***" (ppr binds) $
    pprTrace "*** OUTPUT CORE PROGRAM ***" (ppr binds') $
      return binds'

nukeMatchableFromRule :: CoreRule -> Maybe CoreRule
nukeMatchableFromRule rule@(Rule {}) = Nothing
nukeMatchableFromRule other = Just other

removeMatchableModGutsPass :: ModGuts -> CoreM ModGuts
removeMatchableModGutsPass modguts = do
  binds' <- removeMatchablePass (mg_binds modguts)
  let rules' = mapMaybe nukeMatchableFromRule (mg_rules modguts)
      tcs' = map transformTyCon (mg_tcs modguts)
  return $ modguts {mg_binds = binds', mg_rules = rules', mg_tcs = tcs'}