{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Haskcasting.ExprLang.Ops (
  Perm (.., PermEmpty),
  permTrim,
  permExtend,
  permDeepen,
  permUndeepen,
  permBookkeepers,
  decomposePermBookkeepers,
  Fish (..),
  permFish,
  decomposePerm,
  StackOp (..),
  FuncOp (..),
  Op (..),
  optimizeOps,
  OptM,
  OptT,
  optimizePerm,
  lowerOps,
) where

import Control.Monad (foldM_, forM, forM_, replicateM, void)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.Reader (Reader, ReaderT (ReaderT), ask, runReaderT)
import Control.Monad.ST (ST, runST)
import Data.Bool (bool)
import Data.Foldable (Foldable (toList), fold)
import Data.Function (on)
import Data.Functor (($>))
import Data.List.Index (iall, iforM_, indexed)
import Data.List.NonEmpty qualified as NE
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Vector.Strict qualified as V
import Data.Vector.Strict.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Haskcasting.Compound.Hexcasting (lehmerCode, lehmerCodeMaxLen)
import Haskcasting.Embed (iotaConsideration, iotaIntrospection, iotaRetrospection)
import Haskcasting.ExprLang.Core (BlockOpts (..))
import Haskcasting.Iota (IotaAny, IotaCast (iotaCast), IotaNumber (IotaNumber))
import Haskcasting.Patterns.Hexcasting
import Haskcasting.Util (AnySeqLit (anySeqLit))

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
permTrim (Perm depth perm) = Perm (depth - dropN) (VU.take (pl - dropN) perm)
 where
  pl = VU.length perm
  maxs = VU.prescanl' max (-1) perm
  -- perm = [0,1,0] -> maxs = [-1,0,1]
  dropN = foldr const pl $ dropWhile go [0 .. pl - 1]
  go d =
    let i = pl - d - 1
        p = perm VU.! i
        m = maxs VU.! i
     in p > m && p == depth - d - 1

permExtend :: Int -> Perm -> Perm
permExtend n (Perm d p) =
  if n <= 0
    then Perm d p
    else Perm (d + n) (p <> VU.generate n (+ d))

permDeepen :: Int -> Perm -> Perm
permDeepen n (Perm d p) =
  if n <= 0
    then Perm d p
    else Perm (d + n) (VU.generate n id <> VU.map (+ n) p)

permUndeepen :: Perm -> (Int, Perm)
permUndeepen (Perm depth perm) = (dropN, Perm (depth - dropN) (VU.map (subtract dropN) $ VU.drop dropN perm))
 where
  pl = VU.length perm
  mins = VU.prescanr' min depth perm
  -- perm = [1,0,1] -> mins = [0,1,depth]
  dropN = foldr const pl $ dropWhile go [0 .. pl - 1]
  go d =
    let p = perm VU.! d
        m = mins VU.! d
     in p < m && p == d

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
  GT -> Perm (i + 1) (i `VU.cons` [0 .. i - 1])
  LT -> Perm (i + 1) ([1 .. i] `VU.snoc` 0)
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
  (keep, (permTrim -> Perm d p)) = decomposePermBookkeepers p_
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

type OptT = ReaderT BlockOpts
type OptM = Reader BlockOpts

class HasBlockOpts m where
  getBlockOpts :: m BlockOpts
instance Monad m => HasBlockOpts (OptT m) where
  getBlockOpts = ask

infixr 6 <<>>
(<<>>) :: (Applicative m, Monoid a) => m a -> m a -> m a
(<<>>) = liftA2 (<>)

optimizeOps :: Seq Op -> OptM (Seq Op)
optimizeOps = mapM optimizePermOp . ungroupOps . groupOps

groupOps :: Seq Op -> Seq Op
groupOps = shiftRight 0 PermEmpty Seq.empty . shiftLeft 0 PermEmpty Seq.empty
 where
  opsPerm perm = case permTrim perm of
    PermEmpty -> []
    perm' -> [OpStack $ OpPerm perm']
  -- shift stack ops left/right
  shiftLeft off perm funcs = \case
    Seq.Empty -> opsPerm perm <> funcs
    OpStack op :<| ops ->
      let op' = permDeepen off $ opToPerm op
       in shiftLeft off (perm <> op') funcs ops
    op@(OpFunc (FuncOp _ off' 0)) :<| ops ->
      shiftLeft (off + off') perm (funcs :|> op) ops
    op@(OpFunc _) :<| ops ->
      opsPerm perm
        <> funcs
        <> [op]
        <> shiftLeft 0 PermEmpty Seq.empty ops
  shiftRight off perm funcs = \case
    Seq.Empty -> funcs <> opsPerm perm
    ops :|> OpStack op ->
      let op' = permDeepen off $ opToPerm op
       in shiftRight off (op' <> perm) funcs ops
    ops :|> op@(OpFunc (FuncOp _ 0 off')) ->
      shiftRight (off + off') perm (op :<| funcs) ops
    ops :|> op@(OpFunc _) ->
      shiftRight 0 PermEmpty Seq.empty ops
        <> [op]
        <> funcs
        <> opsPerm perm

infixl 5 `MaybeOpFuncR`
pattern MaybeOpFuncR :: Seq Op -> Maybe FuncOp -> Seq Op
pattern xs `MaybeOpFuncR` x <- (matchOpFuncR -> (xs, x))
  where
    xs `MaybeOpFuncR` Nothing = xs
    xs `MaybeOpFuncR` Just x = xs :|> OpFunc x
matchOpFuncR :: Seq Op -> (Seq Op, Maybe FuncOp)
matchOpFuncR = \case
  ops :|> OpFunc x -> (ops, Just x)
  ops -> (ops, Nothing)

infixr 5 `MaybeOpFuncL`
pattern MaybeOpFuncL :: Maybe FuncOp -> Seq Op -> Seq Op
pattern x `MaybeOpFuncL` xs <- (matchOpFuncL -> (x, xs))
  where
    Nothing `MaybeOpFuncL` xs = xs
    Just x `MaybeOpFuncL` xs = OpFunc x :<| xs
matchOpFuncL :: Seq Op -> (Maybe FuncOp, Seq Op)
matchOpFuncL = \case
  OpFunc x :<| ops -> (Just x, ops)
  ops -> (Nothing, ops)

ungroupOps :: Seq Op -> Seq Op
ungroupOps = eagerUngroupLeft . eagerUngroupRight
 where
  eagerUngroupLeft = \case
    ops :|> OpFunc lo :|> OpStack (opToPerm -> perm) `MaybeOpFuncR` ro -> maybeShift ops lo perm ro
    ops :|> OpStack (opToPerm -> l) :|> OpStack (opToPerm -> r) `MaybeOpFuncR` ro ->
      eagerUngroupLeft (ops :|> OpStack (OpPerm $ l <> r) `MaybeOpFuncR` ro)
    ops :|> op -> eagerUngroupLeft ops :|> op
    Seq.Empty -> Seq.Empty
   where
    maybeShift ops lo perm ro =
      let (depth, perm') = permUndeepen perm
          (lDiff, lMinD) = case lo of
            FuncOp _ l r | r >= l -> (r - l, r)
            _ -> (0, depth + 1)
          (rDiff, rMinD) = case ro of
            Just (FuncOp _ l r) | l >= r -> (l - r, l)
            _ -> (0, depth + 1)
       in if depth >= lMinD && (not $ depth >= rMinD && rDiff > lDiff)
            then eagerUngroupLeft (ops :|> OpStack (OpPerm $ permDeepen (depth - lDiff) perm') :|> OpFunc lo) `MaybeOpFuncR` ro
            else eagerUngroupLeft (ops :|> OpFunc lo) :|> OpStack (OpPerm perm) `MaybeOpFuncR` ro
  eagerUngroupRight = \case
    lo `MaybeOpFuncL` OpStack (opToPerm -> perm) :<| OpFunc ro :<| ops -> maybeShift ops lo perm ro
    lo `MaybeOpFuncL` OpStack (opToPerm -> l) :<| OpStack (opToPerm -> r) :<| ops ->
      eagerUngroupRight (lo `MaybeOpFuncL` OpStack (OpPerm $ l <> r) :<| ops)
    op :<| ops -> op :<| eagerUngroupRight ops
    Seq.Empty -> Seq.Empty
   where
    maybeShift ops lo perm ro =
      let (depth, perm') = permUndeepen perm
          (lDiff, lMinD) = case lo of
            Just (FuncOp _ l r) | r >= l -> (r - l, r)
            _ -> (0, depth + 1)
          (rDiff, rMinD) = case ro of
            FuncOp _ l r | l >= r -> (l - r, l)
            _ -> (0, depth + 1)
       in if depth >= rMinD && (not $ depth >= lMinD && lDiff > rDiff)
            then lo `MaybeOpFuncL` eagerUngroupRight (OpFunc ro :<| OpStack (OpPerm $ permDeepen (depth - rDiff) perm') :<| ops)
            else lo `MaybeOpFuncL` OpStack (OpPerm perm) :<| eagerUngroupRight (OpFunc ro :<| ops)

data Cost = Cost !Int !Int
  deriving (Eq, Ord, Show)

instance Semigroup Cost where
  Cost la lb <> Cost ra rb = Cost (la + ra) (lb + rb)
instance Monoid Cost where
  mempty = Cost 0 0

data PathCost = PathCost {pcSeq :: AnySeq, _pcCost :: !Cost}

pathCostLit :: AnySeqLit a => a -> Cost -> PathCost
pathCostLit s c = PathCost (anySeqLit s) c

instance Semigroup PathCost where
  PathCost lis lc <> PathCost ris rc = PathCost (lis <> ris) (lc <> rc)
instance Monoid PathCost where
  mempty = PathCost mempty mempty
instance Eq PathCost where
  PathCost _ lc == PathCost _ rc = lc == rc
instance Ord PathCost where
  PathCost _ lc `compare` PathCost _ rc = lc `compare` rc

newtype PeepholeM s a = PeepholeM (ReaderT (BlockOpts, VM.MVector s [(Int, PathCost)]) (ST s) a)
  deriving (Functor, Applicative, Monad)

instance HasBlockOpts (PeepholeM s) where
  getBlockOpts = fst <$> PeepholeM ask

execPeepholeM :: Monad m => Int -> (forall s. PeepholeM s a) -> OptT m (V.Vector [(Int, PathCost)])
execPeepholeM n st = do
  opts <- ask
  pure $ V.create $ inner opts st
 where
  inner :: BlockOpts -> PeepholeM s a -> ST s (VM.MVector s [(Int, PathCost)])
  inner opts (PeepholeM st') = do
    edges <- VM.replicate n []
    void $ runReaderT st' (opts, edges)
    pure edges

addEdge :: (Int, Int) -> PathCost -> PeepholeM s ()
addEdge (u, v) pc = PeepholeM $ do
  (_, edges) <- ask
  VM.modify edges ((v, pc) :) u

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
            addEdge (u', v') =<< pathRotation d (v' - u')

        -- FishDup -f
        case gs of
          (((+ 1) -> v', FishDup f') : _)
            | f == f' && (v' - u) >= d ->
                addEdge (v' - d, v') =<< pathFish (FishDup (-f))
          _ -> pure ()
      (fs@((u, FishDup f) NE.:| _), _) -> do
        let d = f + 1
            v = (1 +) $ fst $ NE.last fs
        forM_ ([u .. v - 2] :: [Int]) $ \u' -> do
          forM_ ([u' + 2 .. v] :: [Int]) $ \v' -> do
            addEdge (u', v') =<< pathDupSlice d (v' - u')

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
                addEdge (u, v)
                  =<< pathEmbedInt code
                    <<>> pure
                      ( pathCostLit
                          (iotaSwindlersGambit)
                          (Cost 1 1)
                      )
            pure perm'
      _ -> pure ()

pathEmbedInt :: (HasBlockOpts m, Monad m) => Int -> m PathCost
pathEmbedInt n = do
  opts <- getBlockOpts
  pure $ case opts of
    BlockOpts {boAvoidDynamicPatterns = False}
      | Just iotaNum <- iotaMaybeNumericalReflection n ->
          pathCostLit iotaNum (Cost 1 1)
    BlockOpts {boUseIntroRetro = False} ->
      pathCostLit (iotaConsideration, IotaNumber $ fromIntegral n) (Cost 2 1)
    BlockOpts {boUseIntroRetro = True} ->
      pathCostLit
        ( iotaIntrospection
        , IotaNumber $ fromIntegral n
        , iotaRetrospection
        , iotaFlocksDisintegration
        )
        (Cost 4 1)

pathFish :: (HasBlockOpts m, Monad m) => Fish -> m PathCost
pathFish (Fish i_) = case i_ of
  0 -> pure mempty
  1 -> pure $ pathCostLit iotaJestersGambit (Cost 1 0)
  2 -> pure $ pathCostLit iotaRotationGambit (Cost 1 0)
  -2 -> pure $ pathCostLit iotaRotationGambitII (Cost 1 0)
  i ->
    pathEmbedInt i
      <<>> pure
        ( pathCostLit
            (iotaFishermansGambit)
            (Cost 1 0)
        )
pathFish (FishDup i_) = case i_ of
  0 -> pure $ pathCostLit iotaGeminiDecomposition (Cost 1 0)
  1 -> pure $ pathCostLit iotaProspectorsGambit (Cost 1 0)
  -1 -> pure $ pathCostLit iotaUndertakersGambit (Cost 1 0)
  i ->
    pathEmbedInt i
      <<>> pure
        ( pathCostLit
            (iotaFishermansGambitII)
            (Cost 1 0)
        )

pathBookkeeper :: (HasBlockOpts m, Applicative m) => [Bool] -> m PathCost
pathBookkeeper keep = case NE.nonEmpty keep of
  Nothing -> pure mempty
  Just keep' ->
    pure $
      pathCostLit
        (iotaBookkeepersGambit keep')
        (Cost 1 0)

-- 0,1,2,3,4    1,2,3,4,0
--   \__n__/ -> \__n__/
-- \___d___/    \___d___/
pathRotation :: (HasBlockOpts m, Monad m) => Int -> Int -> m PathCost
pathRotation 0 _ = pure mempty
pathRotation d n_ =
  (fmap fold $ replicateM n $ pathFish $ Fish (d - 1))
    <-> (fmap fold $ replicateM (d - n) $ pathFish $ Fish (-(d - 1)))
 where
  n = n_ `rem` d
  (<->) = liftA2 min

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
pathDupSlice :: (HasBlockOpts m, Monad m) => Int -> Int -> m PathCost
pathDupSlice 0 _ = pure $ mempty
pathDupSlice d 1 = pathFish $ FishDup (d - 1)
pathDupSlice 1 n =
  pathEmbedInt (n + 1)
    <<>> pure
      ( pathCostLit
          (iotaGeminiGambit)
          (Cost 1 0)
      )
pathDupSlice 2 2 = pure $ pathCostLit iotaDioscuriGambit (Cost 1 0)
-- too lazy to do list-based dup
-- pathDupSlice d n | n < d = fold $ replicate n $ pathFish $ FishDup (d - 1)
pathDupSlice d n = fmap fold $ replicateM n $ pathFish $ FishDup (d - 1)

optimizePerm :: Perm -> OptM (AnySeq, Cost)
optimizePerm perm = ReaderT $ \opts -> Identity $ runST $ flip runReaderT opts $ do
  let (keep, fishes) = decomposePerm perm
      (bk, bkc) = case NE.nonEmpty keep of
        Nothing -> ([], mempty)
        Just keep' -> ([iotaCast $ iotaBookkeepersGambit keep'], Cost 1 0)
      fl = length fishes

  edges <- execPeepholeM fl $ mapM_ ($ fishes) peepholes

  let infCost = Cost 1000000 0

  dp <- VM.replicate (fl + 1) (pathCostLit () infCost)
  VM.write dp 0 mempty

  iforM_ fishes $ \u fish -> do
    p <- VM.read dp u
    pFish <- pathFish fish
    VM.modify dp (min (p <> pFish)) (u + 1)
    let edges' = edges V.! u
    forM_ edges' $ \(v, p') -> do
      VM.modify dp (min (p <> p')) v

  PathCost is c <- VM.read dp fl
  if c >= infCost
    then error "didn't find path"
    else pure (bk <> is, bkc <> c)

optimizePermOp :: Op -> OptM Op
optimizePermOp (OpStack (OpPerm perm@(Perm d p))) = do
  (is, _cost) <- optimizePerm perm
  pure $ OpFunc $ FuncOp is d (VU.length p)
optimizePermOp op = pure op

seqFish :: Monad m => Fish -> OptT m AnySeq
seqFish = fmap pcSeq . pathFish

seqBookkeeper :: Monad m => [Bool] -> OptT m AnySeq
seqBookkeeper = fmap pcSeq . pathBookkeeper

lowerOps :: Traversable t => t Op -> OptM AnySeq
lowerOps ops = fmap fold $ forM ops $ \case
  OpStack (OpFish f) -> seqFish f
  OpStack (OpBookkeeper ks) -> seqBookkeeper ks
  OpStack (OpPerm p) ->
    let (keep, fishes) = decomposePerm p
     in seqBookkeeper keep <<>> (fmap fold $ traverse seqFish fishes)
  OpFunc (FuncOp insts _ _) -> pure insts
