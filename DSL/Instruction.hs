{-# LANGUAGE FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , OverlappingInstances
           , PolyKinds
           , TypeOperators
           , UndecidableInstances
  #-}
module DSL.Instruction
  ( (:+:)(InjL,InjR) -- Instruction composition
  , (:<-)(inj)       -- Element instructions
  , (:<=)(coerce)    -- Instruction containment
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

-- | Class of instruction containment.
-- "The instruction types in 'i' are all contained in 'i\''".
class i :<= i' where
  coerce :: i p a -> i' p a

-- A composition 'i' :+: 'j' is contained by 'i\''
-- when both instructions are contained in 'i\''.
instance (i :<- i'
         ,j :<= i'
         )
        => (i :+: j) :<= i' where
  coerce (InjL l) = inj l
  coerce (InjR r) = coerce r

-- Instruction 'i' is an element of 'i\''.
instance (i :<- i')
       => i :<= i' where
  coerce i = inj i

