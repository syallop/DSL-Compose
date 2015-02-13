{-# LANGUAGE FlexibleContexts
           , GADTs
           , PolyKinds
           , RankNTypes
           , TypeOperators
           , UndecidableInstances
  #-}
module DSL.Program
  ( Program(..)
  , ProgramInstr(..)

  , inject
  , Compile(compile)
  , ProgramUsing
  ) where

import DSL.Instruction

import Control.Applicative
import Control.Monad

-- | A Program is a sequence of instructions from 'i' which can
-- be interpreted to produce some return value 'a'.
data ProgramInstr i p a where

  -- | Insert an instruction into the context.
  Instr  :: i p a -> ProgramInstr i p a

  -- | Monadic return.
  Return :: a -> ProgramInstr i p a

  -- | Monadic bind.
  Bind   :: p a -> (a -> p b) -> ProgramInstr i p b

newtype Program i a = Program (ProgramInstr i (Program i) a)

instance Monad (Program i) where
  return     = Program . Return
  ia >>= fab = Program $ Bind ia fab

instance Applicative (Program i) where
  pure = return
  (<*>)  = ap

instance Functor (Program i) where
  fmap = liftM


-- | Inject an instruction into a program containing that instruction type.
inject :: (i :<- i') => i (Program i') a -> Program i' a
inject = Program . Instr . inj




-- | Type of 'Program's that may use an instruction type 'i' as part
-- of their instruction-set.
type ProgramUsing i a = forall i'. (i :<- i') => Program i' a

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

