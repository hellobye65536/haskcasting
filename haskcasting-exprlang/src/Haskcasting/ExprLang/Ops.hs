{-# LANGUAGE OverloadedLists #-}

module Haskcasting.ExprLang.Ops (
  Perm (..),
  permEmpty,
  permTrim,
  permExtend,
  permBookkeepers,
  decomposePermBookkeepers,
  Fish (..),
  permFish,
  decomposePerm,
  StackOp (..),
  Op (..),
  optOps,
  lowerOps,
) where

import Control.Monad (forM_, void)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.ST (runST)
import Data.Bool (bool)
import Data.Functor (($>))
import Data.List.Index (iall)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Sequence (Seq)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Haskcasting.Iota (IotaAny, IotaCast (iotaCast))
import Haskcasting.Patterns.Hexcasting (
  iotaBookkeepersGambit,
  iotaFishermansGambit,
  iotaFishermansGambitII,
  iotaNumericalReflection,
 )

type AnySeq = Seq IotaAny

data Perm = Perm Int (VU.Vector Int)
  deriving (Show, Eq)

instance Semigroup Perm where
  l@(Perm _ld lp) <> r@(Perm rd _rp) = Perm ld' $ VU.backpermute lp' rp'
   where
    Perm ld' lp' = permExtend (rd - VU.length lp) l
    Perm _rd' rp' = permExtend (VU.length lp' - rd) r

instance Monoid Perm where
  mempty = permEmpty

permEmpty :: Perm
permEmpty = Perm 0 []

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
    else Perm (d + n) (p <> [d .. d + n - 1])

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
permFish (Fish 0) = permEmpty
permFish (Fish i) = Perm (i + 1) (i `VU.cons` [0 .. i - 1])
permFish (FishDup i) = Perm (i + 1) (i `VU.cons` [0 .. i])

decomposePerm :: Perm -> ([Bool], [Fish])
decomposePerm p_ = if VU.null p then (keep, []) else (keep, fishes)
 where
  (keep, Perm d p) = decomposePermBookkeepers p_
  fishes = runST $ do
    stackBuf <- VUM.new (d + VU.length p)
    stackRef <-
      newSTRef =<< do
        let stack = VUM.drop (VUM.length stackBuf - d) stackBuf
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
          let stack' = VUM.drop (VUM.length stackBuf - (VUM.length stack + 1)) stackBuf
          writeSTRef stackRef stack'
          VUM.write stack' 0 v
        stackFind v = do
          stack <- readSTRef stackRef
          findRes <- runExceptT $ VUM.iforM_ stack $ \i v' -> if v == v' then throwError i else pure ()
          pure $ either id (error "missing element in stack") findRes
    let elided =
          let n = VU.last p
              ts = VU.drop (VU.length p - (n + 1)) p
              doElide = iall (==) $ VU.toList ts
           in if doElide then n + 1 else 0
        p' = VU.take (VU.length p - elided) p
    seen <- VUM.replicate d False
    VU.forM_ (VU.drop (VU.length p - elided) p) $ \v -> VUM.write seen v True

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

data Op = OpStack StackOp | OpFunc AnySeq Int Int

optOps :: [Op] -> [Op]
optOps ops = undefined

-- where
--  _ = _

-- shiftedForward = foldr shiftForward [] ops
-- shiftForward fn@(OpFunc) = _

-- optStackOps :: Seq StackOp ->

-- opt

-- singlePatternPerms :: [(Perm, IotaPattern)]
-- singlePatternPerms =
--   [ (Perm 2 [1, 0], iotaJestersGambit)
--   , (Perm 3 [2, 0, 1], iotaCast iotaRotationGambit)
--   , (Perm 3 [1, 2, 0], iotaCast iotaRotationGambitII)
--   , (Perm 1 [0, 0], iotaCast iotaGeminiDecomposition)
--   , (Perm 2 [1, 0, 1], iotaCast iotaProspectorsGambit)
--   , (Perm 2 [0, 1, 0], iotaCast iotaUndertakersGambit)
--   , (Perm 2 [0, 1, 0, 1], iotaCast iotaDioscuriGambit)
--   ]

-- lowerPerm :: Perm -> AnySeq
-- lowerPerm perm = bk <> permIotas
--  where
--   (keep, perm') = splitPermBookkeepers perm
--   permIotas = case trimPerm perm' of
--     -- identity
--     Perm 0 [] -> Seq.empty
--     -- single patterns
--     perm''
--       | Just pat <- perm'' `lookup` singlePatternPerms ->
--           Seq.singleton $ iotaCast pat
--     -- dupN
--     Perm 1 p ->
--       Seq.fromList
--         [ iotaCast $ iotaNumericalReflection (length p)
--         , iotaCast iotaGeminiGambit
--         ]
--     -- fish
--     perm''@(Perm d _p)
--       | perm'' == permFish (d - 1) ->
--           Seq.fromList
--             [ iotaCast iotaConsideration
--             , iotaCast $ IotaNumber $ fromIntegral (d - 1)
--             , iotaCast iotaFishermansGambit
--             ]
--     -- fish dup
--     perm''@(Perm d _p)
--       | perm'' == permFishDup (d - 1) ->
--           Seq.fromList
--             [ iotaCast iotaConsideration
--             , iotaCast $ IotaNumber $ fromIntegral (d - 1)
--             , iotaCast iotaFishermansGambitII
--             ]
--     -- swindler's
--     Perm d p
--       | maximum p < d
--       , Just lehmer <- lehmerCode p ->
--           Seq.fromList
--             [ iotaCast iotaConsideration
--             , iotaCast $ IotaNumber $ fromIntegral lehmer
--             , iotaCast iotaSwindlersGambit
--             ]
--     -- tail permutation
--     Perm d p
--       | lp <- length p
--       , drop (lp - d) p == [0 .. d - 1] ->
--           _
--     -- list brute force
--     Perm d p ->
--       let rest = foldr go Seq.empty p
--           go i Seq.Empty =
--             Seq.fromList
--               [ iotaCast $ iotaNumericalReflection i
--               , iotaCast $ iotaSelectionDistillation
--               ]
--           go i s =
--             Seq.fromList
--               [ iotaCast $ iotaGeminiDecomposition
--               , iotaCast $ iotaNumericalReflection i
--               , iotaCast $ iotaSelectionDistillation
--               , iotaCast $ iotaJestersGambit
--               ]
--               <> s
--        in Seq.fromList
--             [ iotaCast $ iotaNumericalReflection d
--             , iotaCast $ iotaFlocksGambit
--             ]
--             <> rest
--   bk = case keep of
--     [] -> Seq.empty
--     (k : ks) -> Seq.singleton $ iotaCast $ iotaBookkeepersGambit (k :| ks)

lowerOps :: [Op] -> AnySeq
lowerOps ops = flip foldMap ops $ \case
  OpStack (OpFish f) -> fish f
  OpStack (OpBookkeeper ks) -> bookkeeper ks
  OpStack (OpPerm p) ->
    let (keep, fishes) = decomposePerm p
     in bookkeeper keep <> foldMap fish fishes
  OpFunc insts _ _ -> insts
 where
  fish (Fish i) = [iotaCast $ iotaNumericalReflection i, iotaCast iotaFishermansGambit]
  fish (FishDup i) = [iotaCast $ iotaNumericalReflection i, iotaCast iotaFishermansGambitII]
  bookkeeper [] = []
  bookkeeper (k : ks) = [iotaCast $ iotaBookkeepersGambit (k :| ks)]
