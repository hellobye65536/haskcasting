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
  call,
  lambdaCall,
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
  --
  -- lowerBlockState,
) where

import Control.Monad (forM_)
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
import Haskcasting.Iota (IotaAny, IotaCast (iotaCast), IotaExec)
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
intro (Fragment is) = Expr $ Intro is $ hListLen @a

call ::
  forall b a blk.
  HListLen b =>
  (forall s. Fragment (HAppendListR a s) (HAppendListR b s)) ->
  Expr blk a ->
  Expr blk b
call (Fragment fun) (Expr arg) = Expr $ Call fun arg $ hListLen @b

lambdaCall ::
  forall a b blk.
  HListLen b =>
  (forall s. Expr blk '[IotaExec (HAppendListR a s) (HAppendListR b s)]) ->
  Expr blk a ->
  Expr blk b
lambdaCall (Expr fun) (Expr arg) = Expr $ LambdaCall fun arg $ hListLen @b

infixr 6 +|+
(+|+) :: Expr blk a -> Expr blk b -> Expr blk (HAppendListR a b)
Expr a +|+ Expr b = Expr $ Merge a b

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

getBsBindingLen :: Monad m => ExprBlockT blk m Int
getBsBindingLen = ExprBlockT $ fmap bsBindingLen $ get

pushBsBinding :: Monad m => (RawExpr, Int) -> ExprBlockT blk m ()
pushBsBinding (e, n) =
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
  off <- getBsBindingLen
  pushBsBinding (expr, hListLen @xs)
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
  ops <- lowerBlockT 0 (hListLen @arg) (hListLen @ret) blk'
  let ops' = optimizeOps $ Seq.fromList ops
      insts = lowerOps ops'

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
  ops <- lowerBlockT (hListLen @cap) (hListLen @arg) (hListLen @ret) blk'
  let ops' = optimizeOps $ Seq.fromList ops
      insts = lowerOps ops'

  let exprBase = Intro insts 1
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

lowerBlockT :: forall m blk. Monad m => Int -> Int -> Int -> ExprBlockT blk m RawExpr -> m [Op]
lowerBlockT capL argL retL blk = fmap lowerBlockState $ flip execStateT blockStateDefault $ unwrapExprBlockT $ do
  pushBsBinding (Intro capsInsts $ argL + capL, argL + capL)
  expr <- blk
  pushBsBinding (expr, retL)
 where
  capsInsts =
    case capL of
      0 -> []
      1 -> [iotaCast iotaConsideration, iotaCast $ iotaBookkeepersGambit [True]]
      _ -> [iotaCast iotaConsideration, iotaCast $ iotaBookkeepersGambit [True], iotaCast iotaFlocksDisintegration]

newtype LowerM s a = LowerM (StateT [Op] (ST s) a)
  deriving (Functor, Applicative, Monad)

liftLower :: ST s a -> LowerM s a
liftLower = LowerM . lift

execLowerM :: forall a. (forall s. LowerM s a) -> [Op]
execLowerM st = reverse $ runST $ inner st
 where
  inner :: forall s'. LowerM s' a -> ST s' [Op]
  inner (LowerM st') = execStateT st' []

lowerOp :: Op -> LowerM s ()
lowerOp op = LowerM $ modify' (op :)

lowerBlockState :: BlockState -> [Op]
lowerBlockState bs = execLowerM $ do
  let BlockState {bsBindings = binds_, bsBindingLen = varCnt} = bs
      binds = reverse binds_
  varUseCnts <- liftLower $ VUM.replicate varCnt (0 :: Int)
  let cntExprVar = \case
        Intro _is _len -> pure ()
        Call _fun arg _len -> cntExprVar arg
        LambdaCall fun arg _len -> cntExprVar fun *> cntExprVar arg
        Merge l r -> cntExprVar l *> cntExprVar r
        Var v -> VUM.modify varUseCnts (+ 1) v
  liftLower $ forM_ binds (cntExprVar . fst)

  vars <- liftLower $ newSTRef 0
  varStack <- liftLower $ newSTRef Seq.empty
  forM_ binds $ \(expr, bindLen) -> do
    lowerExpr varUseCnts varStack expr

    vars' <- liftLower $ readSTRef vars
    liftLower $ modifySTRef' vars (+ bindLen)
    -- [bindLen - 1 .. 0] + vars'
    let newBinds = Seq.fromFunction bindLen (bindLen - 1 + vars' -)
    liftLower $ modifySTRef' varStack $ (newBinds <>)

lowerExpr :: VUM.MVector s Int -> STRef s (Seq Int) -> RawExpr -> LowerM s ()
lowerExpr varUseCnts varStack expr = do
  off <- liftLower $ newSTRef 0
  let go = \case
        Intro is' len -> do
          liftLower $ modifySTRef' off (+ len)
          lowerOp $ OpFunc $ FuncOp is' 0 len
        Call fun arg len -> do
          go arg
          off' <- liftLower $ readSTRef off
          liftLower $ writeSTRef off len
          lowerOp $ OpFunc $ FuncOp fun off' len
        LambdaCall fun arg len -> do
          go arg
          go fun
          off' <- liftLower $ readSTRef off
          liftLower $ writeSTRef off len
          lowerOp $ OpFunc $ FuncOp [iotaCast iotaHermesGambit] off' len
        Merge l r -> liftA2 (<>) (go r) (go l)
        Var v -> do
          uses <- liftLower $ VUM.read varUseCnts v
          off' <- liftLower $ readSTRef off
          vars <- liftLower $ readSTRef varStack
          let vind = fromMaybe (error "missing variable in stack") $ v `Seq.elemIndexL` vars
          stackOp <-
            if
              | uses <= 0 -> error "somehow got to zero uses"
              | uses == 1 -> do
                  liftLower $ writeSTRef varStack $ Seq.deleteAt vind vars
                  pure $ OpFish $ Fish (vind + off')
              | otherwise -> pure $ OpFish $ FishDup (vind + off')
          liftLower $ VUM.modify varUseCnts (subtract 1) v
          liftLower $ writeSTRef off (off' + 1)
          lowerOp $ OpStack stackOp
  go expr

-- lowerBlockState :: BlockState -> Seq Op
-- lowerBlockState BlockState {bsBindings = binds, bsBindingLen = varCnt} = runST $ do
--   os <- newSTRef Seq.empty
--   varUseCnts <- VUM.replicate varCnt (0 :: Int)
--   let cntExprVar = \case
--         Intro _is _len -> pure ()
--         Call _fun arg _len -> cntExprVar arg
--         LambdaCall fun arg _len -> cntExprVar fun *> cntExprVar arg
--         Merge l r -> cntExprVar l *> cntExprVar r
--         Var v -> VUM.modify varUseCnts (+ 1) v
--   forM_ binds (cntExprVar . fst)

--   vars <- newSTRef 0
--   varStack <- newSTRef Seq.empty
--   forM_ binds $ \(expr, bindLen) -> do
--     os' <- lowerExpr varUseCnts varStack expr
--     modifySTRef' os (<> os')

--     vars' <- readSTRef vars
--     modifySTRef' vars (+ bindLen)
--     modifySTRef' varStack $ (Seq.fromFunction bindLen (subtract 1 . (bindLen -) . (+ vars')) <>)
--   readSTRef os

-- lowerExpr :: VUM.MVector s Int -> STRef s (Seq Int) -> RawExpr -> ST s (Seq Op)
-- lowerExpr varUseCnts varStack expr = do
--   off <- newSTRef 0
--   let go = \case
--         Intro is' len -> do
--           modifySTRef' off (+ len)
--           pure $ Seq.singleton $ OpFunc is' 0 len
--         Call fun arg len -> do
--           arg' <- go arg
--           off' <- readSTRef off
--           writeSTRef off len
--           pure (arg' Seq.|> OpFunc fun off' len)
--         LambdaCall fun arg len -> do
--           arg' <- go arg
--           fun' <- go fun
--           off' <- readSTRef off
--           writeSTRef off len
--           pure $ arg' <> fun' Seq.|> OpFunc [iotaCast iotaHermesGambit] off' len
--         Merge l r -> liftA2 (<>) (go r) (go l)
--         Var v -> do
--           uses <- VUM.read varUseCnts v
--           off' <- readSTRef off
--           vars' <- readSTRef varStack
--           let vind = fromMaybe (error "missing variable in stack") $ v `Seq.elemIndexL` vars'
--           perm <-
--             if
--               | uses <= 0 -> error "somehow got to zero uses"
--               | uses == 1 -> do
--                   writeSTRef varStack $ Seq.deleteAt vind vars'
--                   pure $ permFish (vind + off')
--               | otherwise -> pure $ permFishDup (vind + off')
--           VUM.modify varUseCnts (subtract 1) v
--           pure $ Seq.singleton $ OpPerm perm
--   go expr

-- -- foo =
-- --   let n = intro @'[_] $ fragNumericalReflection 0
-- --    in blockTup @'[IotaNumber] @'[IotaList IotaNumber] @'[] n $ \(c, a) -> do
-- --         (c', c'') <- blockBindTup $ call @'[_, _] fragGeminiDecomposition c
-- --         _

-- optOps = _
-- lowerOps = _

-- -- optOps :: Seq Op -> Seq Op
-- -- optOps = undefined

-- -- lowerPerm :: Perm -> AnySeq
-- -- lowerPerm = undefined

-- -- lowerOps :: Seq Op -> AnySeq
-- -- lowerOps ops = flip foldMap ops $ \case
-- --   OpPerm perm -> lowerPerm perm
-- --   OpFunc insts _ _ -> insts
