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

module Control.Coeffect.Bounded where

import           Control.Coeffect
import           Control.Coeffect.Operators

import           Data.Typeable
import           Data.Finite
import           Data.Vector.Sized as V

import           GHC.TypeLits


newtype BndVar (n :: Nat) a =  BndVar {unsafeGetCopies :: Vector n a} 

instance Functor (BndVar n) where
  fmap f (BndVar vec) = BndVar (fmap f vec)

instance Coeffect BndVar where
  type Inv BndVar n m = (KnownNat (n*m), KnownNat n, KnownNat m, (1 <=? n*m) ~ 'True)
  type Unit BndVar = 1
  type Plus BndVar n m = n*m
  
  extract (BndVar vec) = vHead vec 

  extend f (BndVar vec) = BndVar . fmap (f . BndVar) . duplicate $ vec
    where
      duplicate :: forall n m a . ((1 <=? n*m) ~ 'True, KnownNat n, KnownNat m, KnownNat (n*m)) => Vector (n*m) a -> Vector n (Vector m a)
      duplicate vec = V.replicate (V.replicate (vHead vec))


instance CoeffectZip BndVar where
  type CzipInv BndVar n m = (KnownNat n, KnownNat m, KnownNat (n+m))
  type Meet BndVar n m = n + m 
  czip (BndVar v) (BndVar w) = BndVar (V.replicate (x,y))
    where
      x = vHead v
      y = vHead w

data Dollar = Dollar 
data CandyBar = CandyBar 
data Happiness = Happiness deriving (Show)

bound :: KnownNat n => a -> BndVar n a 
bound x = BndVar (V.replicate x)

doubleMoney :: (KnownNat n, KnownNat (n+n)) => BndVar n Dollar -> BndVar (n + n) Dollar 
doubleMoney x = collapse <$> czip x x 
  where
    collapse (Dollar, Dollar) = Dollar

vendCandy :: BndVar 4 Dollar -> CandyBar 
vendCandy _ = CandyBar

beHappy :: BndVar 3 CandyBar -> Happiness 
beHappy _ = Happiness

tryToBeHappy = extract (
  doubleMoney rich
  =>> vendCandy
  =>> beHappy)

rich :: BndVar 6 Dollar
rich = bound Dollar
