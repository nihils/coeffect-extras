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

module Control.Coeffect.Operators where

import           Control.Coeffect
import           Data.Finite
import           Data.Vector.Sized as V


(=>>) :: (Coeffect c, Inv c s t) => c (Plus c s t) a -> (c t a -> b) -> c s b
(=>>) x f = extend f x

vHead x = V.index x (finite 0)
