{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Haskcasting.ExprLang (
  Expr,
  empty,
  intro,
  introUnsafe,
  call,
  callUnsafe,
  cast,
  unsafeCast,
  (%:),
  --
  lambdaCall,
  embedIntroRetro,
  embedConsideration,
  -- block
  BlockOpts (..),
  defaultBlockOpts,
  ExprBlockT,
  ExprBlockM,
  blockBind,
  blockBindHList,
  --
  block,
  blockHList,
  blockT,
  blockHListT,
  blockOpt,
  blockHListOpt,
  blockTOpt,
  blockHListTOpt,
  --
  lambda,
  lambdaHList,
  lambdaT,
  lambdaHListT,
  lambdaOpt,
  lambdaHListOpt,
  lambdaTOpt,
  lambdaHListTOpt,
) where

import Control.Monad (foldM_, forM, forM_)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), runReader)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (MonadState (get), StateT (runStateT), modify')
import Control.Monad.Trans (MonadTrans)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (runIdentity))
import Data.HList (
  HAppendListR,
  HList (HCons, HNil),
 )
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Primitive.MutVar (MutVar, modifyMutVar', newMutVar, readMutVar, writeMutVar)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed.Mutable qualified as VUM

import Haskcasting.Embed (EmbedIntroRetro, iotaConsideration, iotaIntrospection, iotaRetrospection)
import Haskcasting.Embed qualified as Embed
import Haskcasting.ExprLang.Core
import Haskcasting.ExprLang.Ops (
  Fish (..),
  FuncOp (..),
  Op (..),
  StackOp (..),
  lowerOps,
  optimizeOps,
 )
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (IotaAny, IotaCast, IotaExec, IotaList (IotaList))
import Haskcasting.Patterns.Hexcasting (
  iotaBookkeepersGambit,
  iotaFlocksDisintegration,
  iotaFlocksGambit,
  iotaHermesGambit,
  iotaNumericalReflection,
  iotaSurgeonsExaltation,
 )
import Haskcasting.Util (AnySeq, HListLen, anySeqLit, hListLen)

lambdaCall ::
  forall b a blk.
  HListLen b =>
  (forall s. Expr blk '[IotaExec (HAppendListR a s) (HAppendListR b s)]) ->
  Expr blk a ->
  Expr blk b
lambdaCall fun arg = callUnsafe (anySeqLit iotaHermesGambit) (fun %: arg)

embedIntroRetro :: EmbedIntroRetro a => a -> Expr blk '[a]
embedIntroRetro x = intro $ Embed.embedIntroRetro x

embedConsideration :: IotaCast a IotaAny => a -> Expr blk '[a]
embedConsideration x = intro $ Embed.embedConsideration x

-- ==== block typedefs

data BlockState = BlockState
  { bsBindings :: [(RawExpr, Int)]
  , bsBindingLen :: Int
  }
  deriving (Show)

blockStateDefault :: BlockState
blockStateDefault = BlockState {bsBindings = [], bsBindingLen = 0}

newtype ExprBlockT blk m a = ExprBlockT {unwrapExprBlockT :: StateT BlockState m a}
  deriving (Functor, Applicative, Monad, MonadTrans)
type ExprBlockM blk a = ExprBlockT blk Identity a

bsGetBindingLen :: Monad m => ExprBlockT blk m Int
bsGetBindingLen = ExprBlockT $ fmap bsBindingLen $ get

bsPushBinding :: Monad m => (RawExpr, Int) -> ExprBlockT blk m ()
bsPushBinding (e, n) =
  ExprBlockT $
    modify'
      ( \BlockState {bsBindings, bsBindingLen} ->
          BlockState
            { bsBindings = (e, n) : bsBindings
            , bsBindingLen = n + bsBindingLen
            }
      )

type family HTuple (xs :: [Type]) where
  HTuple '[] = ()
  HTuple '[a] = a
  HTuple '[a, b] = (a, b)
  HTuple '[a, b, c] = (a, b, c)
  HTuple '[a, b, c, d] = (a, b, c, d)
  HTuple '[a, b, c, d, e] = (a, b, c, d, e)
  HTuple '[a, b, c, d, e, f] = (a, b, c, d, e, f)
  HTuple '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  HTuple '[a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)

type family ExprSplit blk (xs :: [Type]) = (r :: [Type]) | r -> xs where
  ExprSplit blk '[] = '[]
  ExprSplit blk (x ': xs) = Expr blk '[x] ': ExprSplit blk xs

class ExprSplitTuple (xs :: [Type]) where
  exprSplitTuple :: HList (ExprSplit blk xs) -> HTuple (ExprSplit blk xs)
instance ExprSplitTuple '[] where
  exprSplitTuple HNil = ()
instance ExprSplitTuple '[a] where
  exprSplitTuple (a `HCons` HNil) = a
instance ExprSplitTuple '[a, b] where
  exprSplitTuple (a `HCons` b `HCons` HNil) = (a, b)
instance ExprSplitTuple '[a, b, c] where
  exprSplitTuple (a `HCons` b `HCons` c `HCons` HNil) = (a, b, c)
instance ExprSplitTuple '[a, b, c, d] where
  exprSplitTuple (a `HCons` b `HCons` c `HCons` d `HCons` HNil) = (a, b, c, d)
instance ExprSplitTuple '[a, b, c, d, e] where
  exprSplitTuple (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` HNil) = (a, b, c, d, e)
instance ExprSplitTuple '[a, b, c, d, e, f] where
  exprSplitTuple (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` HNil) = (a, b, c, d, e, f)
instance ExprSplitTuple '[a, b, c, d, e, f, g] where
  exprSplitTuple (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` HNil) = (a, b, c, d, e, f, g)
instance ExprSplitTuple '[a, b, c, d, e, f, g, h] where
  exprSplitTuple (a `HCons` b `HCons` c `HCons` d `HCons` e `HCons` f `HCons` g `HCons` h `HCons` HNil) = (a, b, c, d, e, f, g, h)

class MkExprVars (xs :: [Type]) where
  mkExprVars :: forall blk. Int -> HList (ExprSplit blk xs)
instance MkExprVars '[] where
  mkExprVars _off = HNil
instance (HListLen xs, MkExprVars xs) => MkExprVars (x ': xs) where
  mkExprVars :: forall blk. Int -> HList (ExprSplit blk (x ': xs))
  mkExprVars off = Expr (Var $ off + hListLen @xs) `HCons` mkExprVars @xs @blk off

-- ==== block api

blockBind ::
  forall xs blk m.
  ( Monad m
  , HListLen xs
  , MkExprVars xs
  , ExprSplitTuple xs
  ) =>
  Expr blk xs ->
  ExprBlockT blk m (HTuple (ExprSplit blk xs))
blockBind = fmap (exprSplitTuple @xs @blk) . blockBindHList

blockBindHList ::
  forall xs blk m.
  (Monad m, HListLen xs, MkExprVars xs) =>
  Expr blk xs ->
  ExprBlockT blk m (HList (ExprSplit blk xs))
blockBindHList (Expr expr) = do
  off <- bsGetBindingLen
  bsPushBinding (expr, hListLen @xs)
  pure $ mkExprVars @xs @blk off

type BlockConstraint arg ret =
  ( HListLen arg
  , HListLen ret
  , MkExprVars arg
  , HListLen arg
  )

type LambdaConstraint cap arg ret inits =
  ( HListLen cap
  , HListLen arg
  , HListLen ret
  , HAppendListR cap arg ~ inits
  , MkExprVars inits
  , HListLen inits
  )

block ::
  forall arg ret s.
  ( BlockConstraint arg ret
  , ExprSplitTuple arg
  ) =>
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
block blk = runIdentity $ blockT @arg @ret @s blk

blockHList ::
  forall arg ret s.
  BlockConstraint arg ret =>
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
blockHList blk = runIdentity $ blockHListT @arg @ret @s blk

blockT ::
  forall arg ret s m.
  ( Monad m
  , BlockConstraint arg ret
  , ExprSplitTuple arg
  ) =>
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockT blk = blockHListT @arg @ret @s blk'
 where
  blk' :: forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @arg @blk

blockHListT ::
  forall arg ret s m.
  ( Monad m
  , BlockConstraint arg ret
  ) =>
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockHListT blk = blockHListTOpt @arg @ret @s defaultBlockOpts blk

blockOpt ::
  forall arg ret s.
  ( BlockConstraint arg ret
  , ExprSplitTuple arg
  ) =>
  BlockOpts ->
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
blockOpt opts blk = runIdentity $ blockTOpt @arg @ret @s opts blk

blockHListOpt ::
  forall arg ret s.
  BlockConstraint arg ret =>
  BlockOpts ->
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
blockHListOpt opts blk = runIdentity $ blockHListTOpt @arg @ret @s opts blk

blockTOpt ::
  forall arg ret s m.
  ( Monad m
  , BlockConstraint arg ret
  , ExprSplitTuple arg
  ) =>
  BlockOpts ->
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockTOpt opts blk = blockHListTOpt @arg @ret @s opts blk'
 where
  blk' :: forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @arg @blk

blockHListTOpt ::
  forall arg ret s m.
  ( Monad m
  , BlockConstraint arg ret
  ) =>
  BlockOpts ->
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockHListTOpt opts blk = do
  let blk' = fmap unwrapExpr $ blk @() $ mkExprVars @arg @() 0
  insts <- lowerBlockT opts 0 (hListLen @arg) (hListLen @ret) blk'
  pure $ Fragment insts

lambda ::
  forall cap arg ret s inits blk'.
  ( LambdaConstraint cap arg ret inits
  , ExprSplitTuple inits
  ) =>
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambda cap blk = runIdentity $ lambdaT @cap @arg @ret @s cap blk

lambdaHList ::
  forall cap arg ret s inits blk'.
  LambdaConstraint cap arg ret inits =>
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambdaHList cap blk = runIdentity $ lambdaHListT @cap @arg @ret @s cap blk

lambdaT ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , LambdaConstraint cap arg ret inits
  , ExprSplitTuple inits
  ) =>
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaT cap blk = lambdaHListT @cap @arg @ret @s cap blk'
 where
  blk' :: forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @inits @blk

lambdaHListT ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , LambdaConstraint cap arg ret inits
  ) =>
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaHListT expr blk = lambdaHListTOpt @cap @arg @ret @s defaultBlockOpts expr blk

lambdaOpt ::
  forall cap arg ret s inits blk'.
  ( LambdaConstraint cap arg ret inits
  , ExprSplitTuple inits
  ) =>
  BlockOpts ->
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambdaOpt opts cap blk = runIdentity $ lambdaTOpt @cap @arg @ret @s opts cap blk

lambdaHListOpt ::
  forall cap arg ret s inits blk'.
  LambdaConstraint cap arg ret inits =>
  BlockOpts ->
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambdaHListOpt opts cap blk = runIdentity $ lambdaHListTOpt @cap @arg @ret @s opts cap blk

lambdaTOpt ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , LambdaConstraint cap arg ret inits
  , ExprSplitTuple inits
  ) =>
  BlockOpts ->
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaTOpt opts cap blk = lambdaHListTOpt @cap @arg @ret @s opts cap blk'
 where
  blk' :: forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @inits @blk

lambdaHListTOpt ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , LambdaConstraint cap arg ret inits
  ) =>
  BlockOpts ->
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaHListTOpt opts (Expr cap) blk = do
  let blk' = fmap unwrapExpr $ blk @() $ mkExprVars @inits @() 0
  insts <- lowerBlockT opts (hListLen @cap) (hListLen @arg) (hListLen @ret) blk'

  let exprBase = Intro (anySeqLit (iotaConsideration, IotaList insts)) 1
      exprCap = cap `Merge` Intro (anySeqLit $ iotaNumericalReflection 1) 1 `Merge` exprBase
  pure $ Expr $ case (hListLen @cap) of
    0 -> exprBase
    1 -> Call (anySeqLit iotaSurgeonsExaltation) exprCap 1
    _ ->
      Call
        ( anySeqLit
            ( iotaNumericalReflection (hListLen @cap)
            , iotaFlocksGambit
            , iotaSurgeonsExaltation
            )
        )
        exprCap
        1

-- ==== block lowering

lowerBlockT ::
  forall m blk.
  Monad m =>
  BlockOpts ->
  Int ->
  Int ->
  Int ->
  ExprBlockT blk m RawExpr ->
  m AnySeq
lowerBlockT opts caps args _rets blk = do
  (ret, blockState) <- flip runStateT blockStateDefault $ unwrapExprBlockT $ do
    let capsPlaceholder =
          if boUseIntroRetro opts
            then anySeqLit (iotaIntrospection, iotaBookkeepersGambit [True], iotaRetrospection, iotaFlocksDisintegration)
            else anySeqLit (iotaConsideration, iotaBookkeepersGambit [True])
        capsInsts =
          case caps of
            0 -> Seq.empty
            1 -> capsPlaceholder
            _ -> capsPlaceholder <> anySeqLit iotaFlocksDisintegration
    bsPushBinding (Intro Seq.empty args, args)
    bsPushBinding (Intro capsInsts caps, caps)
    blk
  let ops = lowerBlockState blockState ret
  pure $ flip runReader opts $ lowerOps =<< optimizeOps ops

data LowerState s
  = LowerState
  { lsOps :: MutVar s [Op]
  , lsVarStack :: MutVar s (Seq Int)
  , lsVarUses :: VUM.MVector s Int
  }

newtype LowerM s a = LowerM (ReaderT (LowerState s) (ST s) a)
  deriving (Functor, Applicative, Monad, PrimMonad)

runLowerM :: VUM.MVector s Int -> LowerM s () -> ST s [Op]
runLowerM lsVarUses (LowerM st) = do
  lsOps <- newMutVar []
  lsVarStack <- newMutVar Seq.empty
  runReaderT st $ LowerState {lsOps, lsVarStack, lsVarUses}
  fmap reverse $ readMutVar lsOps

lowerMPushOp :: Op -> LowerM s ()
lowerMPushOp op = LowerM $ do
  LowerState {lsOps} <- ask
  modifyMutVar' lsOps (op :)

lowerMPushVar :: Int -> LowerM s ()
lowerMPushVar v = LowerM $ do
  LowerState {lsVarStack} <- ask
  modifyMutVar' lsVarStack (v Seq.<|)

lowerMGetVarUses :: Int -> LowerM s Int
lowerMGetVarUses v = LowerM $ do
  LowerState {lsVarUses} <- ask
  VUM.read lsVarUses v

lowerMFishVar :: Int -> Int -> LowerM s Fish
lowerMFishVar off v = LowerM $ do
  LowerState {lsVarStack, lsVarUses} <- ask
  lsVarStack' <- readMutVar lsVarStack
  VUM.modify lsVarUses (subtract 1) v
  let i = fromMaybe (error "missing variable in stack") $ v `Seq.elemIndexL` lsVarStack'
  uses <- VUM.read lsVarUses v
  if
    | uses < 0 -> error "ran out of variable uses"
    | uses == 0 -> do
        writeMutVar lsVarStack (Seq.deleteAt i lsVarStack')
        pure $ Fish (i + off)
    | otherwise -> pure $ FishDup (i + off)

lowerBlockState :: BlockState -> RawExpr -> Seq Op
lowerBlockState blockState ret = runST $ do
  let BlockState {bsBindings = (reverse -> binds), bsBindingLen = varCnt} = blockState
  varUses <- VUM.replicate varCnt (0 :: Int)
  let countVarUses = \case
        Intro _is _len -> pure ()
        Call _fun arg _len -> countVarUses arg
        Merge l r -> countVarUses l *> countVarUses r
        Var v -> VUM.modify varUses (+ 1) v
  forM_ binds (countVarUses . fst)
  countVarUses ret

  ops <- runLowerM varUses $ do
    (\go -> foldM_ go 0 binds) $ \vars (expr, vars') ->
      do
        lowerExpr expr
        keep <- forM (take vars' [vars ..]) $ \v ->
          lowerMGetVarUses v >>= \case
            0 -> pure False
            _ -> lowerMPushVar v $> True
        case dropWhile id keep of
          [] -> pure ()
          keep' -> lowerMPushOp $ OpStack $ OpBookkeeper $ reverse keep'
        pure $ vars + vars'
    lowerExpr ret
  pure $ Seq.fromList ops

lowerExpr :: RawExpr -> LowerM s ()
lowerExpr expr = do
  off <- newMutVar 0
  let go = \case
        Intro is' len -> do
          modifyMutVar' off (+ len)
          lowerMPushOp $ OpFunc $ FuncOp is' 0 len
        Call fun arg len -> do
          off_ <- readMutVar off
          go arg
          off' <- readMutVar off
          lowerMPushOp $ OpFunc $ FuncOp fun (off' - off_) len
          writeMutVar off (off_ + len)
        Merge l r -> do
          go r
          go l
        Var v -> do
          off' <- readMutVar off
          fish <- lowerMFishVar off' v
          lowerMPushOp $ OpStack $ OpFish fish
          modifyMutVar' off (+ 1)
  go expr
