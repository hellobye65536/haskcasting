{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.IO qualified as T
import Haskcasting.ExprLang ((%:))
import Haskcasting.ExprLang qualified as E
import Haskcasting.ExprLang.Macros ((%#), (%+))
import Haskcasting.ExprLang.Macros qualified as E
import Haskcasting.Fragment (Fragment, fragAsIota)
import Haskcasting.Iota (Iota (iotaShow), IotaExecId, IotaList, IotaNumber)

p1 :: Fragment (IotaNumber : s) (IotaNumber : s)
p1 = E.block @'[IotaNumber] @'[IotaNumber] pure

p2 :: Fragment (IotaNumber : s) (IotaNumber : s)
p2 = E.block @'[IotaNumber] @'[IotaNumber] $ \n -> do
  n' <- E.blockBind n
  pure n'

p3 :: Fragment (IotaNumber : s) (IotaList IotaNumber : s)
p3 = E.block @'[IotaNumber] @'[IotaList IotaNumber] $ \n -> do
  n' <- E.blockBind n
  pure $ E.cast $ E.mergeList $ (n' %: (n %+ n))

p4 :: Fragment (IotaNumber : s) (IotaList IotaNumber : s)
p4 = E.block @'[IotaNumber] @'[IotaList IotaNumber] $ \n -> do
  n' <- E.blockBind n
  pure $ E.cast $ E.mergeList $ ((n %+ n) %: n')

p5 :: Fragment s (IotaExecId '[IotaNumber] : s)
p5 = E.block @'[] @'[IotaExecId '[IotaNumber]] $ \() -> do
  pure
    $ E.lambda @'[IotaNumber, IotaNumber, IotaNumber] @'[IotaNumber] @'[IotaNumber]
      ((0 %#) %: (1 %#) %: (2 %#))
    $ \(a, b, c, _d) -> do
      pure $ (c %+ b %+ a)

main :: IO ()
main = do
  -- T.putStrLn "\n==== p1 ===="
  -- T.putStrLn $
  --   iotaShow $
  --     fragAsIota p1

  -- T.putStrLn "\n==== p2 ===="
  -- T.putStrLn $
  --   iotaShow $
  --     fragAsIota p2

  -- T.putStrLn "\n==== p3 ===="
  -- T.putStrLn $
  --   iotaShow $
  --     fragAsIota p3

  -- T.putStrLn "\n==== p4 ===="
  -- T.putStrLn $
  --   iotaShow $
  --     fragAsIota p4

  T.putStrLn "\n==== p5 ===="
  T.putStrLn $
    iotaShow $
      fragAsIota p5
