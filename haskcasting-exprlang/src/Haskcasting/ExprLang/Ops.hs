{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Haskcasting.ExprLang.Ops (
  Perm (.., PermEmpty),
  permTrim,
  permExtend,
  permDeepen,
  permBookkeepers,
  decomposePermBookkeepers,
  Fish (..),
  permFish,
  decomposePerm,
  StackOp (..),
  FuncOp (..),
  Op (..),
  optimizeOps,
  optimizePerm,
  lowerOps,
) where

import Control.Monad (foldM_, forM_, void)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT), ask)
import Control.Monad.ST (ST, runST)
import Data.Bool (bool)
import Data.Foldable (Foldable (toList), fold)
import Data.Function (on)
import Data.Functor (($>))
import Data.List.Index (iall, iforM_, indexed)
import Data.List.NonEmpty qualified as NE
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Vector.Strict qualified as V
import Data.Vector.Strict.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Haskcasting.Compound.Hexcasting (lehmerCode, lehmerCodeMaxLen)
import Haskcasting.Embed (iotaConsideration)
import Haskcasting.Iota (IotaAny, IotaCast (iotaCast), IotaNumber (IotaNumber))
import Haskcasting.Patterns.Hexcasting

type AnySeq = Seq IotaAny

data Perm = Perm Int (VU.Vector Int)
  deriving (Show, Eq)

pattern PermEmpty :: Perm
pattern PermEmpty = Perm 0 []

instance Semigroup Perm where
  l@(Perm _ld lp) <> r@(Perm rd _rp) = Perm ld' $ VU.backpermute lp' rp'
   where
    Perm ld' lp' = permExtend (rd - VU.length lp) l
    Perm _rd' rp' = permExtend (VU.length lp' - rd) r

instance Monoid Perm where
  mempty = PermEmpty

permTrim :: Perm -> Perm
permTrim (Perm depth perm) = go depth (VU.length perm - 1)
 where
  maxs = VU.scanl max (-1) perm
  go _ (-1) = Perm 0 []
  go d i =
    let p = perm VU.! i
        m = maxs VU.! i
     in if p > m && p == d - 1
          then go (d - 1) (i - 1)
          else Perm d $ VU.take (i + 1) perm

permExtend :: Int -> Perm -> Perm
permExtend n (Perm d p) =
  if n <= 0
    then Perm d p
    else Perm (d + n) (p <> VU.generate n (+d))

permDeepen :: Int -> Perm -> Perm
permDeepen n (Perm d p) =
  if n <= 0
    then Perm d p
    else Perm (d + n) (VU.generate n id <> VU.map (+ n) p)

permBookkeepers :: [Bool] -> Perm
permBookkeepers keep = Perm (length keep) (VU.fromList $ map snd $ filter fst $ zip keep [0 ..])

decomposePermBookkeepers :: Perm -> ([Bool], Perm)
decomposePermBookkeepers (Perm d p) = runST $ do
  keep <- VUM.replicate d False
  VU.forM_ p $ \i -> VUM.write keep i True

  d' <- VUM.foldl' (\c -> (+ c) . (bool 0 1)) 0 keep
  indMap <- VUM.new d
  void $ VUM.ifoldM' (\j i k -> VUM.write indMap i j $> (bool 0 1 k + j)) (0 :: Int) keep
  p' <- VU.mapM (VUM.read indMap) p

  keep' <- VUM.foldr foldKeepInner [] keep
  pure (keep', Perm d' p')
 where
  foldKeepInner True [] = []
  foldKeepInner k ks = k : ks

data Fish = Fish Int | FishDup Int
  deriving (Show, Eq)

permFish :: Fish -> Perm
permFish (Fish i_) = case i_ `compare` 0 of
  EQ -> PermEmpty
  LT -> Perm (i + 1) (i `VU.cons` [0 .. i - 1])
  GT -> Perm (i + 1) ([1 .. i] `VU.snoc` 0)
 where
  i = abs i_
permFish (FishDup i_) =
  if i_ >= 0
    then Perm (i + 1) (i `VU.cons` [0 .. i])
    else Perm (i + 1) ([0 .. i] `VU.snoc` 0)
 where
  i = abs i_

decomposePerm :: Perm -> ([Bool], [Fish])
decomposePerm p_ = if VU.null p then (keep, []) else (keep, fishes)
 where
  (keep, Perm d p) = decomposePermBookkeepers p_
  vumTakeEnd n v = VUM.drop (VUM.length v - n) v
  vuTakeEnd n v = VU.drop (VU.length v - n) v
  vuDropEnd n v = VU.take (VU.length v - n) v
  fishes = runST $ do
    stackBuf <- VUM.new (d + VU.length p)
    stackRef <-
      newSTRef =<< do
        let stack = vumTakeEnd d stackBuf
        forM_ ([0 .. VUM.length stack - 1] :: [Int]) $ \i -> VUM.unsafeWrite stack i i
        pure stack
    let fish 0 = pure ()
        fish i = do
          stack <- readSTRef stackRef
          v <- VUM.read stack i
          VUM.move
            (VUM.slice 1 i stack)
            (VUM.slice 0 i stack)
          VUM.write stack 0 v
        fishDup i = do
          stack <- readSTRef stackRef
          v <- VUM.read stack i
          let stack' = vumTakeEnd (VUM.length stack + 1) stackBuf
          writeSTRef stackRef stack'
          VUM.write stack' 0 v
        stackFind v = do
          stack <- readSTRef stackRef
          findRes <- runExceptT $ VUM.iforM_ stack $ \i v' -> if v == v' then throwError i else pure ()
          pure $ either id (error "missing element in stack") findRes

    let elided =
          let n = VU.last p
              ts = vuTakeEnd (n + 1) p
              doElide = iall (==) $ VU.toList ts
           in if doElide then n + 1 else 0
        p' = vuDropEnd elided p
    seen <- VUM.replicate d False
    VU.forM_ (vuTakeEnd elided p) $ \v -> VUM.write seen v True

    fmap reverse $ (\go -> VU.foldM' go [] $ VU.reverse p') $ \acc v -> do
      i <- stackFind v
      s <- VUM.read seen v
      VUM.write seen v True
      if not s
        then
          if i == 0
            then pure acc
            else fish i $> Fish i : acc
        else fishDup i $> FishDup i : acc

data StackOp
  = OpFish Fish
  | OpBookkeeper [Bool]
  | OpPerm Perm
  deriving (Show, Eq)

data FuncOp = FuncOp AnySeq Int Int

instance Show FuncOp where
  showsPrec p (FuncOp _s i o) =
    showParen (p > 10) $
      showString "FuncOp <seq> "
        . showsPrec 11 i
        . showString " "
        . showsPrec 11 o

opToPerm :: StackOp -> Perm
opToPerm (OpFish fish) = permFish fish
opToPerm (OpBookkeeper keep) = permBookkeepers keep
opToPerm (OpPerm perm) = perm

data Op = OpStack StackOp | OpFunc FuncOp
  deriving (Show)

optimizeOps :: Seq Op -> Seq Op
optimizeOps = fmap optimizePermOp . groupOps

groupOps :: Seq Op -> Seq Op
groupOps = shiftRight 0 PermEmpty Seq.empty . shiftLeft 0 PermEmpty Seq.empty
 where
  opsPerm perm = case permTrim perm of
    PermEmpty -> []
    perm' -> [OpStack $ OpPerm perm']
  -- shift stack ops left/right
  shiftLeft off perm funcs = \case
    Seq.Empty -> opsPerm perm <> funcs
    (OpStack op) Seq.:<| ops ->
      let op' = permDeepen off $ opToPerm op
       in shiftLeft off (perm <> op') funcs ops
    op@(OpFunc (FuncOp _ off' 0)) Seq.:<| ops ->
      shiftLeft (off + off') perm (funcs Seq.:|> op) ops
    op@(OpFunc _) Seq.:<| ops ->
      opsPerm perm
        <> funcs
        <> [op]
        <> shiftLeft 0 PermEmpty Seq.empty ops
  shiftRight off perm funcs = \case
    Seq.Empty -> funcs <> opsPerm perm
    ops Seq.:|> (OpStack op) ->
      let op' = permDeepen off $ opToPerm op
       in shiftRight off (op' <> perm) funcs ops
    ops Seq.:|> op@(OpFunc (FuncOp _ 0 off')) ->
      shiftRight (off + off') perm (op Seq.:<| funcs) ops
    ops Seq.:|> op@(OpFunc _) ->
      shiftRight 0 PermEmpty Seq.empty ops
        <> [op]
        <> funcs
        <> opsPerm perm

data Cost = Cost !Int !Int
  deriving (Eq, Ord)

instance Semigroup Cost where
  Cost la lb <> Cost ra rb = Cost (la + ra) (lb + rb)
instance Monoid Cost where
  mempty = Cost 0 0

data PathCost = PathCost {pcSeq :: AnySeq, pcCost :: !Cost}

instance Semigroup PathCost where
  PathCost lis lc <> PathCost ris rc = PathCost (lis <> ris) (lc <> rc)
instance Monoid PathCost where
  mempty = PathCost mempty mempty
instance Eq PathCost where
  PathCost _ lc == PathCost _ rc = lc == rc
instance Ord PathCost where
  PathCost _ lc `compare` PathCost _ rc = lc `compare` rc

newtype PeepholeM s a = PeepholeM (ReaderT (VM.MVector s [(Int, PathCost)]) (ST s) a)
  deriving (Functor, Applicative, Monad)

execPeepholeM :: Int -> (forall s. PeepholeM s a) -> V.Vector [(Int, PathCost)]
execPeepholeM n st = V.create $ inner st
 where
  inner :: forall s a. PeepholeM s a -> ST s (VM.MVector s [(Int, PathCost)])
  inner (PeepholeM st') = do
    edges <- VM.replicate n []
    void $ runReaderT st' edges
    pure edges

liftPeepholeST :: ST s a -> PeepholeM s a
liftPeepholeST = PeepholeM . lift

addEdge :: (Int, Int) -> PathCost -> PeepholeM s ()
addEdge (u, v) pc = PeepholeM $ do
  edges <- ask
  lift $ VM.modify edges ((v, pc) :) u

type Peephole s = [Fish] -> PeepholeM s ()

peepholes :: [Peephole s]
peepholes = [consecutive, swindlers]
 where
  consecutive fishes = do
    let groups = NE.groupBy ((==) `on` snd) $ indexed fishes
        pairs = zip groups $ drop 1 $ map toList groups ++ [[]]

    forM_ pairs $ \case
      (fs@((u, Fish f) NE.:| _), gs) -> do
        let d = f + 1
            v = (1 +) $ fst $ NE.last fs
        forM_ ([u .. v - 2] :: [Int]) $ \u' -> do
          forM_ ([u' + 2 .. v] :: [Int]) $ \v' -> do
            addEdge (u', v') $ pathRotation d (v' - u')

        -- FishDup -f
        case gs of
          (((+ 1) -> v', FishDup f') : _)
            | f == f' && (v' - u) >= d ->
                addEdge (v' - d, v') $ pathFish $ FishDup (-f)
          _ -> pure ()
      (fs@((u, FishDup f) NE.:| _), _) -> do
        let d = f + 1
            v = (1 +) $ fst $ NE.last fs
        forM_ ([u .. v - 2] :: [Int]) $ \u' -> do
          forM_ ([u' + 2 .. v] :: [Int]) $ \v' -> do
            addEdge (u', v') $ pathDupSlice d (v' - u')

  swindlers fishes = do
    let cmp (Fish l) (Fish r) = l <= lehmerCodeMaxLen && r <= lehmerCodeMaxLen
        cmp _ _ = False
        groups = NE.groupBy (cmp `on` snd) $ indexed fishes

    forM_ groups $ \case
      fs@((_, Fish f) NE.:| _) | f <= lehmerCodeMaxLen -> do
        forM_ (NE.tails1 fs) $ \((u, f') NE.:| fs') -> do
          (\go -> foldM_ go (permFish f') fs') $ \perm (v_, f'') -> do
            let perm'@(Perm _ p) = perm <> permFish f''
                v = v_ + 1
            case lehmerCode $ VU.toList p of
              Nothing -> pure ()
              Just code ->
                addEdge (u, v) $
                  PathCost
                    [ iotaCast iotaConsideration
                    , iotaCast $ IotaNumber $ fromIntegral code
                    , iotaCast iotaSwindlersGambit
                    ]
                    (Cost 3 2)
            pure perm'
      _ -> pure ()

pathFish :: Fish -> PathCost
pathFish (Fish i_) = case i_ of
  0 -> mempty
  1 -> PathCost [iotaCast iotaJestersGambit] (Cost 1 0)
  2 -> PathCost [iotaCast iotaRotationGambit] (Cost 1 0)
  -2 -> PathCost [iotaCast iotaRotationGambitII] (Cost 1 0)
  i ->
    PathCost
      [ iotaCast $ iotaNumericalReflection i
      , iotaCast iotaFishermansGambit
      ]
      (Cost 2 1)
pathFish (FishDup i_) = case i_ of
  0 -> PathCost [iotaCast iotaGeminiDecomposition] (Cost 1 0)
  1 -> PathCost [iotaCast iotaProspectorsGambit] (Cost 1 0)
  -1 -> PathCost [iotaCast iotaUndertakersGambit] (Cost 1 0)
  i ->
    PathCost
      [ iotaCast $ iotaNumericalReflection i
      , iotaCast iotaFishermansGambitII
      ]
      (Cost 2 1)

pathBookkeeper :: [Bool] -> PathCost
pathBookkeeper keep = case NE.nonEmpty keep of
  Nothing -> mempty
  Just keep' ->
    PathCost
      [iotaCast $ iotaBookkeepersGambit keep']
      (Cost 1 0)

-- 0,1,2,3,4    1,2,3,4,0
--   \__n__/ -> \__n__/
-- \___d___/    \___d___/
pathRotation :: Int -> Int -> PathCost
pathRotation 0 _ = mempty
pathRotation d n_ =
  (fold $ replicate n $ pathFish $ Fish (d - 1))
    `min` (fold $ replicate (d - n) $ pathFish $ Fish (-(d - 1)))
 where
  n = n_ `rem` d

-- chunked rotation, todo
-- 0,1,2,3 -2> [0,1],2,3 -2> [[0,1],2,3] -1> [0,1],[2,3] -1> [2,3],[0,1] -1> [2,3,0,1] -1> 2,3,0,1
-- 0,1,2,3 -2> [0,1],2,3 -2> 2,3,[0,1] -2> [2,3],[0,1] -> ...

-- 0,1,2,3,4    1,2,3,4,0,1,2,3,4
--   \__n__/ -> \__n__/
-- \___d___/            \___d___/
--
--     0,1,2,3,4    3,4,0,1,2,3,4,0,1,2,3,4
-- \_____n_____/ -> \_____n_____/
--     \___d___/                  \___d___/
pathDupSlice :: Int -> Int -> PathCost
pathDupSlice 0 _ = mempty
pathDupSlice d 1 = pathFish $ FishDup (d - 1)
pathDupSlice 1 n =
  PathCost
    [ iotaCast $ iotaNumericalReflection (n + 1)
    , iotaCast iotaGeminiGambit
    ]
    (Cost 2 1)
pathDupSlice 2 2 = PathCost [iotaCast iotaDioscuriGambit] (Cost 1 0)
-- too lazy to do list-based dup
-- pathDupSlice d n | n < d = fold $ replicate n $ pathFish $ FishDup (d - 1)
pathDupSlice d n = fold $ replicate n $ pathFish $ FishDup (d - 1)

optimizePerm :: Perm -> (AnySeq, Cost)
optimizePerm perm = runST $ do
  let (keep, fishes) = decomposePerm perm
      (bk, bkc) = case NE.nonEmpty keep of
        Nothing -> ([], mempty)
        Just keep' -> ([iotaCast $ iotaBookkeepersGambit keep'], Cost 1 0)
      fl = length fishes

  let edges = execPeepholeM fl $ mapM_ ($ fishes) peepholes

  dp <- VM.replicate (fl + 1) (PathCost Seq.empty (Cost 1000000 0))
  VM.write dp 0 mempty

  iforM_ fishes $ \u fish -> do
    p <- VM.read dp u
    VM.modify dp (min (p <> pathFish fish)) (u + 1)
    let edges' = edges V.! u
    forM_ edges' $ \(v, p') -> do
      VM.modify dp (min (p <> p')) v

  PathCost is c <- VM.read dp fl
  if c >= Cost 1000000 0
    then error "didn't find path"
    else pure (bk <> is, bkc <> c)

optimizePermOp :: Op -> Op
optimizePermOp (OpStack (OpPerm perm@(Perm d p))) =
  let (is, _cost) = optimizePerm perm
   in OpFunc $ FuncOp is d (VU.length p)
optimizePermOp op = op

seqFish :: Fish -> AnySeq
seqFish = pcSeq . pathFish

seqBookkeeper :: [Bool] -> AnySeq
seqBookkeeper = pcSeq . pathBookkeeper

lowerOps :: Foldable t => t Op -> AnySeq
lowerOps ops = flip foldMap ops $ \case
  OpStack (OpFish f) -> seqFish f
  OpStack (OpBookkeeper ks) -> seqBookkeeper ks
  OpStack (OpPerm p) ->
    let (keep, fishes) = decomposePerm p
     in seqBookkeeper keep <> foldMap seqFish fishes
  OpFunc (FuncOp insts _ _) -> insts
