module Data.Record.Mutator 
  ( Mutator
  , mutate
  , set
  , modify
  ) where

import Prelude

import Data.Monoid (class Monoid)
import Data.Monoid.Endo.StackSafe (Endo, applyEndo, endo)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)

newtype Mutator r = Mutator (Endo (Record r))

derive instance newtypeMutator :: Newtype (Mutator r) _
derive newtype instance semigroupMutator :: Semigroup (Mutator r)
derive newtype instance monoidMutator :: Monoid (Mutator r)

-- | Internal constructor.
mutator :: forall r. (Record r -> Record r) -> Mutator r
mutator = Mutator <<< endo

foreign import copyRecord :: forall r1. Record r1 -> Record r1

foreign import unsafeSet
  :: forall a r
   . String -> a -> Record r -> Record r

foreign import unsafeModify
  :: forall a r
   . String -> (a -> a) -> Record r -> Record r

-- | Mutate a copy of a record.
mutate :: forall r. Mutator r -> Record r -> Record r
mutate (Mutator f) = applyEndo f <<< copyRecord

-- | Mutate by setting a field's value.
set
  :: forall l a r r'
   . RowCons l a r' r
  => IsSymbol l
  => SProxy l -> a -> Mutator r
set l a = mutator \r -> unsafeSet (reflectSymbol l) a r

-- | Mutate by updating a field's value using a function.
modify
  :: forall l a r r'
   . RowCons l a r' r
  => IsSymbol l
  => SProxy l -> (a -> a) -> Mutator r
modify l f = mutator \r -> unsafeModify (reflectSymbol l) f r
