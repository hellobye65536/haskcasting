{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskcasting.ExprLang.Core (
  Expr (..),
  empty,
  intro,
  introUnsafe,
  call,
  callUnsafe,
  cast,
  unsafeCast,
  (%:),
  BlockOpts (..),
  defaultBlockOpts,
  --
  RawExpr (..),
) where

import Data.HList (HAppendListR)
import Data.Kind (Type)
import Data.Sequence qualified as Seq
import Haskcasting.Fragment (Fragment (Fragment))
import Haskcasting.Iota (IotaAny, IotaCast)
import Haskcasting.Util (HListLen, hListLen)

type AnySeq = Seq.Seq IotaAny

data RawExpr
  = Intro AnySeq Int
  | Call AnySeq RawExpr Int
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
    (Merge l r) ->
      showParen (p > 6) $
        showsPrec 7 l
          . showString " %: "
          . showsPrec 6 r
    (Var v) ->
      showParen (p > 10) $
        showString "Var "
          . showsPrec 11 v

data Expr (blk :: Type) (as :: [Type]) = Expr {unwrapExpr :: RawExpr}
  deriving (Show)

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

infixr 6 %:
(%:) :: Expr blk a -> Expr blk b -> Expr blk (HAppendListR a b)
Expr a %: Expr b = Expr $ Merge a b

class ExprCast bs as where
  cast :: Expr blk as -> Expr blk bs
  cast = Expr . unwrapExpr
instance ExprCast '[] '[]
instance (IotaCast a b, ExprCast bs as) => ExprCast (b ': bs) (a ': as)

class ExprUnsafeCast bs as where
  unsafeCast :: Expr blk as -> Expr blk bs
  unsafeCast = Expr . unwrapExpr
instance ExprUnsafeCast '[] '[]
instance ExprUnsafeCast bs as => ExprUnsafeCast (b ': bs) (a ': as)

data BlockOpts = BlockOpts
  { boUseIntroRetro :: Bool
  , boAvoidDynamicPatterns :: Bool
  }
defaultBlockOpts :: BlockOpts
defaultBlockOpts =
  BlockOpts
    { boUseIntroRetro = False
    , boAvoidDynamicPatterns = False
    }
