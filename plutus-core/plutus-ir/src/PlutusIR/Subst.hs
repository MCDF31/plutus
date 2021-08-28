module PlutusIR.Subst
    ( uniquesTerm
    , uniquesType
    , fvTerm
    , ftvTerm
    , fvBinding
    , ftvBinding
    , ftvTy
    ) where

import qualified PlutusCore.Core.Type as PLC
import qualified PlutusCore.Name      as PLC
import           PlutusCore.Subst     (ftvTy, uniquesType)

import           PlutusIR.Core

import           Control.Lens
import           Data.List.NonEmpty   as NE
import           Data.Set             as Set
import           Data.Set.Lens        (setOf)

uniquesTerm
    :: PLC.HasUniques (Term tyname name uni fun ann)
    => Term tyname name uni fun ann -> Set PLC.Unique
uniquesTerm = setOf termUniquesDeep

-- | Get all the free term variables in a PIR term.
fvTerm :: forall tyname name uni fun ann.
         Ord name => Term tyname name uni fun ann -> Set name
fvTerm = f
  where
    f (Let a NonRec (b :| bs) tIn) =
        fvBinding b <>
        ( (case nonEmpty bs of
            Just nbs -> f (Let a NonRec nbs tIn)
            Nothing  -> f tIn)
            \\ setOf bindingNames b
        )
    f (Let _ Rec bs tIn) =
        (f tIn <> foldMap fvBinding bs) \\ setOf (traversed.bindingNames) bs

    f (LamAbs _ n _ t) = delete n (f t)
    f (Apply _ t1 t2)  = f t1 <> f t2
    f (Var _ n)        = singleton n
    f (TyAbs _ _ _ t)  = f t
    f (TyInst _ t _)   = f t
    f (Unwrap _ t)     = f t
    f (IWrap _ _ _ t)  = f t
    f Constant{}       = mempty
    f Builtin{}        = mempty
    f Error{}          = mempty

-- | Get all the free type variables in a PIR term.
ftvTerm :: forall tyname name uni fun ann.
        Ord tyname => Term tyname name uni fun ann -> Set tyname
ftvTerm = f
  where
    f (Let a NonRec (b :| bs) tIn) =
        ftvBinding b <>
        ( (case NE.nonEmpty bs of
            Just nbs -> f (Let a NonRec nbs tIn)
            Nothing  -> f tIn)
            \\ setOf bindingTyNames b
        )
    f (Let _ Rec bs tIn) =
        (f tIn <> foldMap ftvBinding bs) \\ setOf (traversed.bindingTyNames) bs
    f (TyAbs _ ty _ t)    = delete ty $ f t
    f (LamAbs _ _ ty t)   = ftvTy ty <> f t
    f (Apply _ t1 t2)     = f t1 <> f t2
    f (TyInst _ t ty)     = f t <> ftvTy ty
    f (Unwrap _ t)        = f t
    f (IWrap _ pat arg t) = ftvTy pat <> ftvTy arg <> f t
    f (Error _ ty)        = ftvTy ty
    f Var{}               = mempty
    f Constant{}          = mempty
    f Builtin{}           = mempty

-- | Get all the free variables in a PIR single let-binding.
fvBinding :: Ord name => Binding tyname name uni fun ann -> Set name
fvBinding b = mconcat $ fvTerm <$> ( b^..bindingSubterms)

-- | Get all the free type variables in a PIR single let-binding.
ftvBinding :: Ord tyname => Binding tyname name uni fun ann -> Set tyname
ftvBinding b = mconcat (fmap ftvTerm ( b^..bindingSubterms)
                        ++
                        fmap ftvTy ( b^..bindingSubtypes)
                       )
