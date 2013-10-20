{-# LANGUAGE ExistentialQuantification #-}

module Data.Existential where

data Some f = forall x . Some (f x)
