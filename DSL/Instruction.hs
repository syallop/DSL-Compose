{-# LANGUAGE FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , OverlappingInstances
           , PolyKinds
           , RankNTypes
           , TypeOperators
           , UndecidableInstances
           , IncoherentInstances
  #-}
{-|
Module     : DSL.Instruction
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Functions for working with plain instruction types.
 -}
module DSL.Instruction
  (-- * Instruction composition
    (:+:)(InjL,InjR)

  -- ** Element instructions
  , (:<-)(inj)

  -- ** 'Subset' instructions
  , (:<=)(coerce)

  -- * Misc
  , MapProgram(mapProgram)
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

-- | Class of instruction types 'i' whose base program type can
-- be mapped to another program type.
class MapProgram i where
  mapProgram :: (forall b. p b -> p' b) -> i p a -> i p' a

instance (MapProgram i, MapProgram j)
      => MapProgram (i :+: j) where
  mapProgram f p = case p of
    InjL l -> InjL $ mapProgram f l
    InjR r -> InjR $ mapProgram f r

