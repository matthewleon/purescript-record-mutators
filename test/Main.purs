module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (range)
import Data.Foldable (class Foldable, foldMap)
import Data.Int (even)
import Data.Record.Mutator (modify, mutate)
import Data.Symbol (SProxy(..))
import Test.Assert (ASSERT, assert)

countPred :: forall f a
   . Foldable f
  => (a -> Boolean) -> f a -> { trues :: Int, falses :: Int }
countPred pred as = mutate (foldMap mkMutator as) { trues: 0, falses: 0 }
  where
  mkMutator x = if pred x then incrTrues else incrFalses
  incrTrues = modify (SProxy :: SProxy "trues") (_ + 1)
  incrFalses = modify (SProxy :: SProxy "falses") (_ + 1)

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  let rec = countPred even $ range 0 1000000
  assert $ rec.trues == 500001
  assert $ rec.falses == 500000
