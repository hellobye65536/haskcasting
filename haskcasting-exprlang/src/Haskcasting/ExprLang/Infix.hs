module Haskcasting.ExprLang.Infix ((%+)) where

import Data.HList (HAppendListR)
import Haskcasting.ExprLang (Expr, (+|+))
import Haskcasting.Patterns.Hexcasting (ExprAdditiveDistillation (exprAdditiveDistillation))

infixr 5 %+
(%+) :: ExprAdditiveDistillation (HAppendListR a b) bs => Expr blk b -> Expr blk a -> Expr blk bs
l %+ r = exprAdditiveDistillation $ r +|+ l
