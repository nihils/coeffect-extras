{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Control.Coeffect.Cache where

import           Control.Coeffect
import           Control.Coeffect.Operators

import           Data.Finite
import           Data.Vector.Sized as V
import           Data.Maybe
import           Data.Typeable

import           GHC.TypeLits


newtype CacheVar (n :: Nat) a = CacheVar {unsafeGetHistory :: Vector (n+1) a}

instance Functor (CacheVar n) where
  fmap f (CacheVar vec) = CacheVar (fmap f vec)

instance Coeffect CacheVar where
  type Inv CacheVar n m = (KnownNat ((n+m)+1), KnownNat (n+1), KnownNat (m+1))  
  type Unit CacheVar = 0
  type Plus CacheVar n m = n+m
  
  extract (CacheVar vec) = V.last vec
  extend f (CacheVar vec) = CacheVar . fmap (f . CacheVar) . duplicate $ vec  
    where
      -- FIXME: Use a typeclass induction or some other method instead of conversion to lists
      duplicate :: forall n m a . (KnownNat (n+1), KnownNat (m+1), KnownNat ((n+m)+1)) => Vector ((n+m)+1) a -> Vector (n+1) (Vector (m+1) a)
      duplicate vec = fromJust . V.fromList . snd $ go (V.toList vec,[])
        where
          go :: ([a], [Vector (m+1) a]) -> ([a],[Vector (m+1) a])  
          go ([],produced) = ([], produced)
          go (consumed, produced) = go (rest, (fromJust . V.fromList $ x):produced)
            where
              at = fromIntegral (natVal (Proxy :: Proxy (m+1)))
              (rest, x) = Prelude.splitAt at consumed
  
