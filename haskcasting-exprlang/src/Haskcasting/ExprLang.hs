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
  lambdaCall,
  cast,
  unsafeCast,
  (+|+),
  ExprBlockT,
  ExprBlockM,
  blockBind,
  blockBindTup,
  block,
  blockTup,
  blockT,
  blockTupT,
  lambda,
  lambdaTup,
  lambdaT,
  lambdaTupT,
) where

import Control.Monad (foldM_, forM_)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (MonadState (get), MonadTrans (lift), StateT, execStateT, modify')
import Data.Functor.Identity (Identity (runIdentity))
import Data.HList (
  HAppendListR,
  HLength,
  HList (HCons, HNil),
  HNat2Nat,
 )
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.STRef (
  STRef,
  modifySTRef',
  newSTRef,
  readSTRef,
  writeSTRef,
 )
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.TypeLits (KnownNat)
import Haskcasting.Compound.Hexcasting (natValInt)
import Haskcasting.Embed (iotaConsideration)
import Haskcasting.ExprLang.Ops (
  Fish (..),
  FuncOp (..),
  Op (..),
  StackOp (..),
  lowerOps,
  optimizeOps,
 )
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (IotaAny, IotaCast (iotaCast), IotaExec, IotaList (IotaList))
import Haskcasting.Patterns.Hexcasting (
  iotaBookkeepersGambit,
  iotaFlocksDisintegration,
  iotaFlocksGambit,
  iotaHermesGambit,
  iotaNumericalReflection,
  iotaSurgeonsExaltation,
 )

type AnySeq = Seq.Seq IotaAny

data RawExpr
  = Intro AnySeq Int
  | Call AnySeq RawExpr Int
  | LambdaCall RawExpr RawExpr Int
  | Merge RawExpr RawExpr
  | Var Int

instance Show RawExpr where
  showsPrec p = \case
    (Intro _f l) ->
      showParen (p > 10) $
        showString "Intro <seq> "
          . showsPrec 11 l
    (Call _f a l) ->
      showParen (p > 10) $
        showString "Call <seq> "
          . showsPrec 11 a
          . showString " "
          . showsPrec 11 l
    (LambdaCall f a l) ->
      showParen (p > 10) $
        showString "LambdaCall "
          . showsPrec 11 f
          . showString " "
          . showsPrec 11 a
          . showString " "
          . showsPrec 11 l
    (Merge l r) ->
      showParen (p > 6) $
        showsPrec 7 l
          . showString " +|+ "
          . showsPrec 6 r
    (Var v) ->
      showParen (p > 10) $
        showString "Var "
          . showsPrec 11 v

data Expr (blk :: Type) (as :: [Type]) = Expr {unwrapExpr :: RawExpr}
  deriving (Show)

type HListLen xs = KnownNat (HNat2Nat (HLength xs))
hListLen :: forall (xs :: [Type]). HListLen xs => Int
hListLen = natValInt @(HNat2Nat (HLength xs))

empty :: Expr blk '[]
empty = Expr $ Intro Seq.empty 0

intro :: forall a blk. HListLen a => (forall s. Fragment s (HAppendListR a s)) -> Expr blk a
intro (Fragment is) = introUnsafe is

introUnsafe :: forall a blk. HListLen a => AnySeq -> Expr blk a
introUnsafe is = Expr $ Intro is $ hListLen @a

call ::
  forall b a blk.
  HListLen b =>
  (forall s. Fragment (HAppendListR a s) (HAppendListR b s)) ->
  Expr blk a ->
  Expr blk b
call (Fragment fun) = callUnsafe fun

callUnsafe ::
  forall a b blk.
  HListLen b =>
  AnySeq ->
  Expr blk a ->
  Expr blk b
callUnsafe fun (Expr arg) = Expr $ Call fun arg $ hListLen @b

lambdaCall ::
  forall b a blk.
  HListLen b =>
  (forall s. Expr blk '[IotaExec (HAppendListR a s) (HAppendListR b s)]) ->
  Expr blk a ->
  Expr blk b
lambdaCall (Expr fun) (Expr arg) = Expr $ LambdaCall fun arg $ hListLen @b

infixr 6 +|+
(+|+) :: Expr blk a -> Expr blk b -> Expr blk (HAppendListR a b)
Expr a +|+ Expr b = Expr $ Merge a b

class ExprCast as bs where
  cast :: Expr blk as -> Expr blk bs
  cast = Expr . unwrapExpr
instance ExprCast '[] '[]
instance (IotaCast a b, ExprCast as bs) => ExprCast (a ': as) (b ': bs)

unsafeCast :: (HLength as ~ HLength bs) => Expr blk as -> Expr blk bs
unsafeCast = Expr . unwrapExpr

-- ==== block typedefs

data BlockState = BlockState
  { bsBindings :: [(RawExpr, Int)]
  , bsBindingLen :: Int
  }
  deriving (Show)

blockStateDefault :: BlockState
blockStateDefault = BlockState {bsBindings = [], bsBindingLen = 0}

newtype ExprBlockT blk m a = ExprBlockT {unwrapExprBlockT :: StateT BlockState m a}
  deriving (Functor, Applicative, Monad)
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

type family ExprSplit blk (xs :: [Type]) = (r :: [Type]) | r -> xs where
  ExprSplit blk '[] = '[]
  ExprSplit blk (x ': xs) = Expr blk '[x] ': ExprSplit blk xs

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

block ::
  forall arg ret s.
  ( HListLen arg
  , HListLen ret
  , MkExprVars arg
  , HListLen arg
  ) =>
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
block blk = runIdentity $ blockT @arg @ret @s blk

blockTup ::
  forall arg ret s.
  ( HListLen arg
  , HListLen ret
  , MkExprVars arg
  , HListLen arg
  , ExprSplitTuple arg
  ) =>
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)) ->
  Fragment (HAppendListR arg s) (HAppendListR ret s)
blockTup blk = runIdentity $ blockT @arg @ret @s blk'
 where
  blk' :: forall blk. HList (ExprSplit blk arg) -> ExprBlockM blk (Expr blk ret)
  blk' = blk . exprSplitTuple @arg @blk

blockT ::
  forall arg ret s m.
  ( Monad m
  , HListLen arg
  , HListLen ret
  , MkExprVars arg
  , HListLen arg
  ) =>
  (forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockT blk = do
  let blk' = fmap unwrapExpr $ blk @() $ mkExprVars @arg @() 0
  insts <- lowerBlockT 0 (hListLen @arg) (hListLen @ret) blk'
  pure $ Fragment insts

blockTupT ::
  forall arg ret s m.
  ( Monad m
  , HListLen arg
  , HListLen ret
  , MkExprVars arg
  , HListLen arg
  , ExprSplitTuple arg
  ) =>
  (forall blk. HTuple (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Fragment (HAppendListR arg s) (HAppendListR ret s))
blockTupT blk = blockT @arg @ret @s blk'
 where
  blk' :: forall blk. HList (ExprSplit blk arg) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @arg @blk

lambda ::
  forall cap arg ret s inits blk'.
  ( HListLen cap
  , HListLen arg
  , HListLen ret
  , HAppendListR cap arg ~ inits
  , MkExprVars inits
  , HListLen inits
  ) =>
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambda cap blk = runIdentity $ lambdaT @cap @arg @ret @s cap blk

lambdaTup ::
  forall cap arg ret s inits blk'.
  ( HListLen cap
  , HListLen arg
  , HListLen ret
  , HAppendListR cap arg ~ inits
  , MkExprVars inits
  , HListLen inits
  , ExprSplitTuple inits
  ) =>
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)) ->
  Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)]
lambdaTup cap blk = runIdentity $ lambdaT @cap @arg @ret @s cap blk'
 where
  blk' :: forall blk. HList (ExprSplit blk inits) -> ExprBlockM blk (Expr blk ret)
  blk' = blk . exprSplitTuple @inits @blk

lambdaT ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , HListLen cap
  , HListLen arg
  , HListLen ret
  , HAppendListR cap arg ~ inits
  , MkExprVars inits
  , HListLen inits
  ) =>
  Expr blk' cap ->
  (forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaT (Expr cap) blk = do
  let blk' = fmap unwrapExpr $ blk @() $ mkExprVars @inits @() 0
  insts <- lowerBlockT (hListLen @cap) (hListLen @arg) (hListLen @ret) blk'

  let exprBase = Intro [iotaCast iotaConsideration, iotaCast $ IotaList insts] 1
      exprCap = cap `Merge` Intro [iotaCast $ iotaNumericalReflection 1] 1 `Merge` exprBase
  pure $ Expr $ case (hListLen @cap) of
    0 -> exprBase
    1 -> Call [iotaCast iotaSurgeonsExaltation] exprCap 1
    _ ->
      Call
        [ iotaCast $ iotaNumericalReflection (hListLen @cap)
        , iotaCast iotaFlocksGambit
        , iotaCast iotaSurgeonsExaltation
        ]
        exprCap
        1

lambdaTupT ::
  forall cap arg ret s inits blk' m.
  ( Monad m
  , HListLen cap
  , HListLen arg
  , HListLen ret
  , HAppendListR cap arg ~ inits
  , MkExprVars inits
  , HListLen inits
  , ExprSplitTuple inits
  ) =>
  Expr blk' cap ->
  (forall blk. HTuple (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)) ->
  m (Expr blk' '[IotaExec (HAppendListR arg s) (HAppendListR ret s)])
lambdaTupT cap blk = lambdaT @cap @arg @ret @s cap blk'
 where
  blk' :: forall blk. HList (ExprSplit blk inits) -> ExprBlockT blk m (Expr blk ret)
  blk' = blk . exprSplitTuple @inits @blk

-- ==== block lowering

lowerBlockT :: forall m blk. Monad m => Int -> Int -> Int -> ExprBlockT blk m RawExpr -> m AnySeq
lowerBlockT caps args rets blk = do
  blockState <- flip execStateT blockStateDefault $ unwrapExprBlockT $ do
    let capsInsts =
          case caps of
            0 -> []
            1 -> [iotaCast iotaConsideration, iotaCast $ iotaBookkeepersGambit [True]]
            _ -> [iotaCast iotaConsideration, iotaCast $ iotaBookkeepersGambit [True], iotaCast iotaFlocksDisintegration]
    bsPushBinding (Intro capsInsts $ args + caps, args + caps)
    expr <- blk
    bsPushBinding (expr, rets)
  let ops = lowerBlockState blockState
      ops' = optimizeOps ops
      insts = lowerOps ops'
  pure insts

data LowerState s
  = LowerState
  { lsOps :: STRef s [Op]
  , lsVarStack :: STRef s (Seq Int)
  , lsVarUses :: VUM.MVector s Int
  }

newtype LowerM s a = LowerM (ReaderT (LowerState s) (ST s) a)
  deriving (Functor, Applicative, Monad)

runLowerM :: VUM.MVector s Int -> LowerM s () -> ST s [Op]
runLowerM lsVarUses (LowerM st) = do
  lsOps <- newSTRef []
  lsVarStack <- newSTRef Seq.empty
  runReaderT st $ LowerState {lsOps, lsVarStack, lsVarUses}
  fmap reverse $ readSTRef lsOps

lowerMLift :: ST s a -> LowerM s a
lowerMLift = LowerM . lift

lowerMPushOp :: Op -> LowerM s ()
lowerMPushOp op = LowerM $ do
  LowerState {lsOps} <- ask
  lift $ modifySTRef' lsOps (op :)

lowerMPushVar :: Int -> LowerM s ()
lowerMPushVar v = LowerM $ do
  LowerState {lsVarStack} <- ask
  lift $ modifySTRef' lsVarStack (v Seq.<|)

lowerMGetVarUses :: Int -> LowerM s Int
lowerMGetVarUses v = LowerM $ do
  LowerState {lsVarUses} <- ask
  lift $ VUM.read lsVarUses v

lowerMFishVar :: Int -> Int -> LowerM s Fish
lowerMFishVar off v = LowerM $ do
  LowerState {lsVarStack, lsVarUses} <- ask
  lsVarStack' <- lift $ readSTRef lsVarStack
  uses <- lift $ VUM.read lsVarUses v
  let i = fromMaybe (error "missing variable in stack") $ v `Seq.elemIndexL` lsVarStack'
  VUM.modify lsVarUses (subtract 1) v
  if
    | uses <= 0 -> error "somehow got to zero uses"
    | uses == 1 -> do
        lift $ modifySTRef' lsVarStack (Seq.deleteAt i)
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
        LambdaCall fun arg _len -> countVarUses fun *> countVarUses arg
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
  off <- lowerMLift $ newSTRef 0
  let go = \case
        Intro is' len -> do
          lowerMLift $ modifySTRef' off (+ len)
          lowerMPushOp $ OpFunc $ FuncOp is' 0 len
        Call fun arg len -> do
          go arg
          off' <- lowerMLift $ readSTRef off
          lowerMPushOp $ OpFunc $ FuncOp fun off' len
          lowerMLift $ writeSTRef off len
        LambdaCall fun arg len -> do
          go arg
          go fun
          off' <- lowerMLift $ readSTRef off
          lowerMPushOp $ OpFunc $ FuncOp [iotaCast iotaHermesGambit] off' len
          lowerMLift $ writeSTRef off len
        Merge l r -> do
          go r
          go l
        Var v -> do
          off' <- lowerMLift $ readSTRef off
          fish <- lowerMFishVar off' v
          lowerMPushOp $ OpStack $ OpFish fish
          lowerMLift $ modifySTRef' off (+ 1)
  go expr
