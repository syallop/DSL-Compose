{-# LANGUAGE FlexibleContexts
           , GADTs
           , PolyKinds
           , RankNTypes
           , TypeOperators
           , UndecidableInstances
  #-}
module DSL.Program
  ( Program(..)

  , inject
  , Compile(compile)
  , ProgramUsing
  , coerceProgram
  ) where

import DSL.Instruction

import Control.Applicative
import Control.Monad

-- | A Program is a sequence of instructions from 'i' which can
-- be interpreted to produce some return value 'a'.
data Program i a where

  -- | Insert an instruction into the context.
  Instr  :: i a -> Program i a

  -- | Monadic return.
  Return :: a -> Program i a

  -- | Monadic bind.
  Bind   :: Program i a -> (a -> Program i b) -> Program i b

instance Monad (Program i) where
  return     = Return
  ia >>= fab = Bind ia fab

instance Applicative (Program i) where
  pure = return
  (<*>)  = ap

instance Functor (Program i) where
  fmap = liftM

-- | Inject an instruction into a program containing that instruction type.
inject :: (i :<- i') => i a -> Program i' a
inject = Instr . inj

-- | Coerce a 'Program' on an instruction type into a program
-- on a larger/ compatible instruction type.
coerceProgram :: (i :<= j) => Program i a -> Program j a
coerceProgram i = case i of
  Return a
    -> Return a

  Bind ma f
    -> Bind (coerceProgram ma) (coerceProgram . f)

  Instr i
    -> Instr $ coerce i

-- | Type of 'Program's that may use an instruction type 'i' as part
-- of their instruction-set.
type ProgramUsing i a = forall i'. (i :<- i') => Program i' a

-- | Class of types 't a' that can be canonically compiled to
-- produce an 'a'.
class Compile t where
  compile :: t a -> a

-- Instruction compositions can be compiled when each composed instruction type
-- can be compiled.
instance (Compile i
         ,Compile j
         ) =>
         Compile (i :+: j) where
    compile (InjL l) = compile l
    compile (InjR r) = compile r

instance Compile i => Compile (Program i) where
  compile is = case is of
      Instr i  -> compile i
      Return a -> a
      Bind m f -> compile $ f $ compile m 

