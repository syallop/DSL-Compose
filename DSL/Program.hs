{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , GADTs
           , GeneralizedNewtypeDeriving
           , LambdaCase
           , MultiParamTypeClasses
           , PolyKinds
           , RankNTypes
           , TypeOperators
           , UndecidableInstances
  #-}
{-|
Module     : DSL.Program
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Promote instructions into a monadic 'Program' type representing
a sequence of instructions with a recursive base-program type of itself.
 -}
module DSL.Program
  ( Program(..)
  , ProgramInstr(..)

  , inject
  , embed
  , Compile(compile)
  , ProgramUsing
  , ProgramUsingIn
  ) where

import DSL.Instruction

import Control.Monad

-- | A ProgramInstr is a sequence of instructions from 'i' which can
-- be interpreted to produce some return value 'a'.
-- 'p' gives the base program type.
data ProgramInstr i p a where

  -- | Insert an instruction into the context.
  Instr  :: i p a -> ProgramInstr i p a

  -- | Monadic return.
  Return :: a -> ProgramInstr i p a

  -- | Monadic bind.
  Bind   :: p a -> (a -> p b) -> ProgramInstr i p b

-- how to change a ProgramInstr's underlying base program type.
instance MapProgram i
      => MapProgram (ProgramInstr i) where
  mapProgram f = \case
    Instr  i  -> Instr $ mapProgram f i
    Return a  -> Return a
    Bind p fp -> Bind (f p) (\a -> f (fp a))

-- | A Program is a sequence of instructions from 'i' which can be interpreted to
-- produce some return value 'a'.
--
-- (The base program-type fed to all contained instructions is, recursivly, the 'Program i' itself.
-- Therefore 'Program i's instructions may contain and operate on other 'Program i's.)
newtype Program i a = Program (ProgramInstr i (Program i) a)

instance Monad (Program i) where
  ia >>= fab = Program $ Bind ia fab

instance Applicative (Program i) where
  pure = Program . Return
  (<*>)  = ap

instance Functor (Program i) where
  fmap = liftM

instance MonadFail (Program i) where
  fail = error

-- | Inject an instruction into a program containing that instruction type.
inject :: (i :<- i') => i (Program i') a -> Program i' a
inject = Program . Instr . inj

-- | Embed a 'Program' on an instruction type into a program on a
-- larger/ compatible instruction type.
--
-- The smaller instruction type must know how to transform its uses
-- of its base program type with a MapProgram instance.
embed :: (i :<= j, MapProgram i) => Program i a -> Program j a
embed (Program p) = Program $ case mapProgram embed p of
  Instr i   -> Instr (coerce i)
  Return a  -> Return a
  Bind ma f -> Bind ma f


-- | Type of 'Program's that may use an instruction type 'i' as part
-- of their instruction-set.
type ProgramUsing i a = forall i'. ProgramUsingIn i i' a

-- | Type of 'Program's that may use an instruction type 'i' as part of a larger
-- instruction-set 'i\''.
type ProgramUsingIn i i' a = (i :<- i') => Program i' a

-- | Class of types 't a' that can be canonically compiled to
-- produce an 'a'.
class Compile t where
  compile :: t a -> a

-- Instruction compositions can be compiled when each composed instruction type
-- can be compiled.
instance (Compile (i p)
         ,Compile (j p)
         ) =>
         Compile ((i :+: j) p) where
    compile (InjL l) = compile l
    compile (InjR r) = compile r

instance (Compile (i p), Compile p)
      => Compile (ProgramInstr i p) where
  compile is = case is of
      Instr i  -> compile i
      Return a -> a
      Bind m f -> compile $ f $ compile m

instance Compile (i (Program i))
      => Compile (Program i) where
  compile (Program p) = compile p

