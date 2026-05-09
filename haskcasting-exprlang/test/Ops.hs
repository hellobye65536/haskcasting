{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.Vector.Unboxed qualified as VU
import Haskcasting.ExprLang.Ops (
  Fish (..),
  Perm (Perm, PermEmpty),
  decomposePerm,
  decomposePermBookkeepers,
  permBookkeepers,
  permDeepen,
  permExtend,
  permFish,
  permTrim,
  permUndeepen,
 )
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), NonNegative (NonNegative), Positive (Positive), chooseInt, sized)

instance Arbitrary Perm where
  arbitrary = sized $ \case
    0 -> pure PermEmpty
    pl -> do
      d <- chooseInt (1, 16)
      p <- VU.replicateM pl (chooseInt (0, d - 1))
      pure $ Perm d p

  shrink PermEmpty = []
  shrink (Perm d p) = [PermEmpty] <> shrinkD <> shrinkP
   where
    maxP = VU.foldl' max (-1) p
    shrinkD = [Perm d' p | d' <- [maxP + 1 .. d - 1]]
    shrinkP = Perm d <$> (map VU.fromList $ shrink $ VU.toList p)

main :: IO ()
main = hspec $ do
  describe "permTrim" $ do
    it "permTrim samples" $ do
      permTrim (Perm 0 []) `shouldBe` Perm 0 []
      permTrim (Perm 1 [0]) `shouldBe` Perm 0 []
      permTrim (Perm 1 [0, 0]) `shouldBe` Perm 1 [0, 0]

    prop "permTrim is idempotent" $ \(p :: Perm) ->
      permTrim (permTrim p) `shouldBe` permTrim p

    prop "permTrim identity should be empty" $ \(n :: Int) ->
      permTrim (permExtend n PermEmpty) `shouldBe` Perm 0 []

    prop "permTrim drop should be same" $ \(Positive d :: Positive Int) ->
      permTrim (Perm d []) `shouldBe` Perm d []

    prop "permExtend (permTrim perm) `shouldBe` perm" $ \(perm :: Perm) ->
      let Perm d p = perm
          Perm d' p' = permTrim perm
       in permExtend (d - d') (Perm d' p') `shouldBe` Perm d p

  describe "permExtend" $ do
    prop "permTrim (permExtend perm) `shouldBe` permTrim perm" $ \(p :: Perm, n :: Int) ->
      permTrim (permExtend n p) `shouldBe` permTrim p

  describe "permUndeepen" $ do
    it "permUndeepen samples" $ do
      permUndeepen (Perm 0 []) `shouldBe` (0, Perm 0 [])
      permUndeepen (Perm 1 [0]) `shouldBe` (1, Perm 0 [])
      permUndeepen (Perm 1 [0, 0]) `shouldBe` (0, Perm 1 [0, 0])
      permUndeepen (Perm 2 [0, 1]) `shouldBe` (2, Perm 0 [])
      permUndeepen (Perm 3 [0, 1, 1]) `shouldBe` (1, Perm 2 [0, 0])

    prop "permDeepen . permUndeepen should be identity" $ \(p :: Perm) ->
      let (n, p') = permUndeepen p
       in permTrim (permDeepen n p') `shouldBe` permTrim p

  describe "Monoid Perm" $ do
    prop "perm <> identity `shouldBe` perm" $ \(p :: Perm, n :: Int) ->
      let ident = permExtend n PermEmpty
       in permTrim (p <> ident) `shouldBe` permTrim p

    prop "identity <> perm `shouldBe` perm" $ \(p :: Perm, n :: Int) ->
      let ident = permExtend n PermEmpty
       in permTrim (ident <> p) `shouldBe` permTrim p

  describe "decompose" $ do
    prop "decomposePermBookkeepers is correct" $ \(p :: Perm) ->
      let (keep, p') = decomposePermBookkeepers p
       in permTrim (permBookkeepers keep <> p') `shouldBe` permTrim p

    it "decomposePerm samples" $ do
      decomposePerm (Perm 0 []) `shouldBe` ([], [])
      decomposePerm (Perm 4 [1, 0, 2]) `shouldBe` ([True, True, True, False], [Fish 1])

    prop "decomposePerm is correct" $ \(p :: Perm) ->
      let (keep, fishes) = decomposePerm p
       in permTrim (permBookkeepers keep <> foldMap permFish fishes) `shouldBe` permTrim p

    prop "decomposePerm of permFish Fish" $ \(Positive n :: Positive Int) ->
      decomposePerm (permFish $ Fish n) `shouldBe` ([], [Fish n])

    prop "decomposePerm of permFish FishDup" $ \(NonNegative n :: NonNegative Int) ->
      decomposePerm (permFish $ FishDup n) `shouldBe` ([], [FishDup n])
