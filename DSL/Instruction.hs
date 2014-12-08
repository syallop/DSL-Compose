{-# LANGUAGE FlexibleInstances
            , GADTs
            , MultiParamTypeClasses
            , TypeOperators
  #-}
module DSL.Instruction
  ( (:+:)(InjL,InjR) -- Instruction composition
  , (:<-)(inj)       -- Element instructions
  ) where

-- | Composition of two instruction types 'i' and 'j'
-- , both parameterised over a common base program type 'p'
-- returning a result type 'a'.
data (i :+: j) p a where
  InjL :: i p a -> (i :+: j) p a
  InjR :: j p a -> (i :+: j) p a
infixr :+:

-- | Class of sub-instructions.
-- 'i' is/ is contained within 'i\''.
class i :<- i' where
  inj :: i p a -> i' p a -- ^ Inject a term in 'i' into a term in 'i\''.

-- Instructions are trivially themselves
instance i :<- i where
  inj = id

-- Instruction contained in left side of a composition.
instance i :<- (i :+: i') where
  inj = InjL

-- Instruction appears somewhere within the right side of a composition.
instance (i :<- i'')
      => i :<- (i' :+: i'') where
  inj = InjR . inj

