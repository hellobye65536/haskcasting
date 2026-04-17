{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

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
  blockBindTup,
  --
  block,
  blockTup,
  blockT,
  blockTupT,
  blockOpt,
  blockTupOpt,
  blockTOpt,
  blockTupTOpt,
  --
  lambda,
  lambdaTup,
  lambdaT,
  lambdaTupT,
  lambdaOpt,
  lambdaTupOpt,
  lambdaTOpt,
  lambdaTupTOpt,
) where

import Control.Monad (foldM_, forM_)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), runReader)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (MonadState (get), MonadTrans (lift), StateT, execStateT, modify')
import Data.Functor.Identity (Identity (runIdentity))
import Data.HList (
  HAppendListR,
  HList (HCons, HNil),
 )
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed.Mutable qualified as VUM

import Haskcasting.Embed (EmbedIntroRetro, iotaConsideration)
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

import Control.Monad.Primitive (PrimMonad)
import Data.Primitive.MutVar (MutVar, modifyMutVar', newMutVar, readMutVar, writeMutVar)
import Haskcasting.Embed qualified as Embed
import Haskcasting.ExprLang.Core

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
  (Monad m, HListLen xs, MkExprVars xs) =>
  Expr blk xs ->
  ExprBlockT blk m (HList (ExprSplit blk xs))
blockBind (Expr expr) = do
  off <- bsGetBindingLen
  bsPushBinding (expr, hListLen @xs)
  pure $ mkExprVars @xs @blk off

blockBindTup ::
  forall xs blk m.
  ( Monad m
  , HListLen xs
  , MkExprVars xs
  , ExprSplitTuple xs
  ) =>
  Expr blk xs ->
  ExprBlockT blk m (HTuple (ExprSplit blk xs))
blockBindTup = fmap (exprSplitTuple @xs @blk) . blockBind

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
  BlockConstraint arg ret =>
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
block blk = runIdentity $ blockT @arg @ret @s blk

blockTup ::
  forall arg ret s.
  ( BlockConstraint arg ret
  , ExprSplitTuple arg
  ) =>
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
blockTup blk = runIdentity $ blockTupT @arg @ret @s blk

blockT ::
  forall arg ret s m.
  ( Monad m
  , BlockConstraint arg ret
  ) =>
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockT blk = blockTOpt @arg @ret @s defaultBlockOpts blk

blockTupT ::
  forall arg ret s m.
  ( Monad m
  , BlockConstraint arg ret
  , ExprSplitTuple arg
  ) =>
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockTupT blk = blockT @arg @ret @s blk'
 where
  blk' :: forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @arg @blk

blockOpt ::
  forall arg ret s.
  BlockConstraint arg ret =>
  BlockOpts ->
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
blockOpt opts blk = runIdentity $ blockTOpt @arg @ret @s opts blk

blockTupOpt ::
  forall arg ret s.
  ( BlockConstraint arg ret
  , ExprSplitTuple arg
  ) =>
  BlockOpts ->
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
blockTupOpt opts blk = runIdentity $ blockTupTOpt @arg @ret @s opts blk

blockTOpt ::
  forall arg ret s m.
  ( Monad m
  , BlockConstraint arg ret
  ) =>
  BlockOpts ->
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockTOpt opts blk = do
  let blk' = fmap unwrapExpr $ blk @() $ mkExprVars @arg @() 0
  insts <- lowerBlockT opts 0 (hListLen @arg) (hListLen @ret) blk'
  pure $ Fragment insts

blockTupTOpt ::
  forall arg ret s m.
  ( Monad m
  , BlockConstraint arg ret
  , ExprSplitTuple arg
  ) =>
  BlockOpts ->
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockTupTOpt opts blk = blockTOpt @arg @ret @s opts blk'
 where
  blk' :: forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @arg @blk

lambda ::
  forall cap arg ret s inits blk'.
  LambdaConstraint cap arg ret inits =>
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambda cap blk = runIdentity $ lambdaT @cap @arg @ret @s cap blk

lambdaTup ::
  forall cap arg ret s inits blk'.
  ( LambdaConstraint cap arg ret inits
  , ExprSplitTuple inits
  ) =>
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambdaTup cap blk = runIdentity $ lambdaTupT @cap @arg @ret @s cap blk

lambdaT ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , LambdaConstraint cap arg ret inits
  ) =>
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaT expr blk = lambdaTOpt @cap @arg @ret @s defaultBlockOpts expr blk

lambdaTupT ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , LambdaConstraint cap arg ret inits
  , ExprSplitTuple inits
  ) =>
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaTupT cap blk = lambdaT @cap @arg @ret @s cap blk'
 where
  blk' :: forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @inits @blk

lambdaOpt ::
  forall cap arg ret s inits blk'.
  LambdaConstraint cap arg ret inits =>
  BlockOpts ->
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambdaOpt opts cap blk = runIdentity $ lambdaTOpt @cap @arg @ret @s opts cap blk

lambdaTupOpt ::
  forall cap arg ret s inits blk'.
  ( LambdaConstraint cap arg ret inits
  , ExprSplitTuple inits
  ) =>
  BlockOpts ->
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambdaTupOpt opts cap blk = runIdentity $ lambdaTupTOpt @cap @arg @ret @s opts cap blk

lambdaTOpt ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , LambdaConstraint cap arg ret inits
  ) =>
  BlockOpts ->
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaTOpt opts (Expr cap) blk = do
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

lambdaTupTOpt ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , LambdaConstraint cap arg ret inits
  , ExprSplitTuple inits
  ) =>
  BlockOpts ->
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaTupTOpt opts cap blk = lambdaTOpt @cap @arg @ret @s opts cap blk'
 where
  blk' :: forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @inits @blk

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
lowerBlockT opts caps args rets blk = do
  blockState <- flip execStateT blockStateDefault $ unwrapExprBlockT $ do
    let capsInsts =
          case caps of
            0 -> Seq.empty
            1 -> anySeqLit (iotaConsideration, iotaBookkeepersGambit [True])
            _ -> anySeqLit (iotaConsideration, iotaBookkeepersGambit [True], iotaFlocksDisintegration)
    bsPushBinding (Intro capsInsts $ args + caps, args + caps)
    expr <- blk
    bsPushBinding (expr, rets)
  let ops = lowerBlockState blockState
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
  uses <- VUM.read lsVarUses v
  let i = fromMaybe (error "missing variable in stack") $ v `Seq.elemIndexL` lsVarStack'
  VUM.modify lsVarUses (subtract 1) v
  if
    | uses <= 0 -> error "somehow got to zero uses"
    | uses == 1 -> do
        lift $ modifyMutVar' lsVarStack (Seq.deleteAt i)
        pure $ Fish (i + off)
    | otherwise -> pure $ FishDup (i + off)

lowerBlockState :: BlockState -> Seq Op
lowerBlockState bs = runST $ do
  let BlockState {bsBindings = binds_, bsBindingLen = varCnt} = bs
      binds = reverse binds_
  varUses <- VUM.replicate varCnt (0 :: Int)
  let countVarUses = \case
        Intro _is _len -> pure ()
        Call _fun arg _len -> countVarUses arg
        Merge l r -> countVarUses l *> countVarUses r
        Var v -> VUM.modify varUses (+ 1) v
  forM_ binds (countVarUses . fst)

  ops <- runLowerM varUses $ (\go -> foldM_ go 0 binds) $ \vars (expr, vars') ->
    do
      lowerExpr expr
      forM_ (take vars' [vars ..]) $ \v ->
        lowerMGetVarUses v >>= \case
          0 -> pure ()
          _ -> lowerMPushVar v
      pure $ vars + vars'
  pure $ Seq.fromList ops

lowerExpr :: RawExpr -> LowerM s ()
lowerExpr expr = do
  off <- newMutVar 0
  let go = \case
        Intro is' len -> do
          modifyMutVar' off (+ len)
          lowerMPushOp $ OpFunc $ FuncOp is' 0 len
        Call fun arg len -> do
          go arg
          off' <- readMutVar off
          lowerMPushOp $ OpFunc $ FuncOp fun off' len
          writeMutVar off len
        Merge l r -> do
          go r
          go l
        Var v -> do
          off' <- readMutVar off
          fish <- lowerMFishVar off' v
          lowerMPushOp $ OpStack $ OpFish fish
          modifyMutVar' off (+ 1)
  go expr
