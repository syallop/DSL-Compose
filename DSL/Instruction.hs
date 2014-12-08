{-# LANGUAGE  GADTs
            , TypeOperators
  #-}
module DSL.Instruction
  ( (:+:)(InjL,InjR) -- Instruction composition
  ) where

-- | Composition of two instruction types 'i' and 'j'
-- , both parameterised over a common base program type 'p'
-- returning a result type 'a'.
data (i :+: j) p a where
  InjL :: i p a -> (i :+: j) p a
  InjR :: j p a -> (i :+: j) p a
infixr :+:

