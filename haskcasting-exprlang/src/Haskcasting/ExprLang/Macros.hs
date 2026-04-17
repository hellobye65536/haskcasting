module Haskcasting.ExprLang.Macros ((%+), (%==), (%/=), (%<), exprList) where

import Data.HList (HAppendListR)
import Haskcasting.ExprLang (Expr, (%:))
import Haskcasting.ExprLang qualified as E
import Haskcasting.ExprLang.Core (Expr (Expr, unwrapExpr), RawExpr (Call, Merge))
import Haskcasting.Iota (Iota, IotaList)
import Haskcasting.Patterns.Hexcasting (
  ExprAdditiveDistillation (exprAdditiveDistillation),
  ExprEqualityDistillation (exprEqualityDistillation),
  ExprInequalityDistillation (exprInequalityDistillation),
  ExprMaximusDistillation (exprMaximusDistillation),
  ExprSinglesPurification (exprSinglesPurification),
  exprVacantReflection,
  iotaFlocksGambit,
  iotaNumericalReflection,
 )
import Haskcasting.Util (AnySeqLit (anySeqLit))

infixr 5 %+
(%+) :: ExprAdditiveDistillation (HAppendListR a b) bs => Expr blk a -> Expr blk b -> Expr blk bs
l %+ r = exprAdditiveDistillation $ l %: r

infix 4 %==
(%==) :: ExprEqualityDistillation (HAppendListR a b) bs => Expr blk a -> Expr blk b -> Expr blk bs
l %== r = exprEqualityDistillation $ l %: r

infix 4 %/=
(%/=) :: ExprInequalityDistillation (HAppendListR a b) bs => Expr blk a -> Expr blk b -> Expr blk bs
l %/= r = exprInequalityDistillation $ l %: r

infix 4 %<
(%<) :: ExprMaximusDistillation (HAppendListR a b) bs => Expr blk a -> Expr blk b -> Expr blk bs
l %< r = exprMaximusDistillation $ l %: r

exprList :: forall a blk. Iota a => [Expr blk '[a]] -> Expr blk '[IotaList a]
exprList [] = E.cast exprVacantReflection
exprList [a] = E.unsafeCast $ exprSinglesPurification a
exprList as =
  let inner = foldl1 (flip Merge) $ map unwrapExpr as
   in Expr $ Call (anySeqLit (iotaNumericalReflection $ length as, iotaFlocksGambit)) inner 1
