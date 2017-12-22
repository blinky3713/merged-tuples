{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib
    ( someFunc
    ) where

import Data.Kind
import Data.Tagged (Tagged(..))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (CmpNat, Nat)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- [Tagged "b" b, Tagged "c" c, Tagged "a" a, Tagged "d" d]
-- Proxy [("a", 1), ("b", 2), ("c", 3), ("d", 4)]
-- [Tagged "a" a, Tagged b "b", Tagged "c" c, Tagged "d" d]

class Sort (xs :: [*]) where
  type Sort' xs :: [*]
  sort :: HList xs -> HList (Sort' xs)

instance Sort '[] where
  type Sort' '[] = '[]
  sort HNil = HNil

instance (Sort xs, Insert x (Sort' xs)) => Sort (x : xs) where
  type Sort' (x : xs) = Insert' x (Sort' xs)
  sort (x :< xs) = insert x (sort xs)

class Insert (x :: *) (xs :: [*]) where
  type Insert' x xs :: [*]
  insert :: x -> HList xs -> HList (Insert' x xs)

instance Insert x '[] where
  type Insert' x '[] = '[x]
  insert x HNil = x :< HNil

instance InsertCmp (CmpNat n m) (Tagged n x) (Tagged m y) ys => Insert (Tagged n x) (Tagged m y : ys) where
  type Insert' (Tagged n x) (Tagged m y : ys) = InsertCmp' (CmpNat n m) (Tagged n x) (Tagged m y) ys
  insert (x :: Tagged n x) ((y :: Tagged m y) :< ys) = insertCmp (Proxy :: Proxy (CmpNat n m)) x y ys

class InsertCmp (b :: Ordering) (x :: *) (y :: *) (ys :: [*]) where
  type InsertCmp' b x y ys :: [*]
  insertCmp :: Proxy (b :: Ordering) -> x -> y -> HList ys -> HList (InsertCmp' b x y ys)

instance InsertCmp 'LT x y ys where
  type InsertCmp' 'LT x y ys = x : (y : ys)
  insertCmp _ x y ys = x :< y :< ys

instance Insert x ys => InsertCmp 'GT x y ys where
  type InsertCmp' 'GT x y ys = y : Insert' x ys
  insertCmp _ x y ys = y :< insert x ys

data HList :: [*] -> * where
  HNil :: HList '[]
  (:<) :: a -> HList as -> HList (a : as)

infixr 0 :<

class UnTag t ut | t -> ut where
  unTag :: HList t -> HList ut

instance UnTag '[] '[] where
  unTag a = a

instance UnTag ts uts => UnTag (Tagged n a : ts) (a : uts) where
  unTag (Tagged a :< ts) = a :< unTag ts

as :: HList '[Tagged 2 String, Tagged 1 Int, Tagged 3 Bool]
as = Tagged "hello" :< Tagged 0 :< Tagged False :< HNil

bs :: HList '[Int, String, Bool]
bs = unTag . sort $ as
