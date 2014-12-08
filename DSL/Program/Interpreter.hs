{-# LANGUAGE FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , PolyKinds
           , RankNTypes
           , TypeOperators
  #-}
module DSL.Program.Interpreter
  (Interpreter(..)
  ,Interpreters((:*:))
  ,interpretWith
  ,interpretProgramWith
  ) where

import DSL.Instruction
import DSL.Program

-- | An Interpreter interprets instructions (with any base program type)
-- , producing a result in 'm'.
newtype Interpreter i m = Interpreter {unInterpreter :: forall a p. i p a -> m a}

-- | Composition of interpreters.
data Interpreters i m where
  (:*:) :: InterpretWith int i' m
        => Interpreter i m
        -> int i' m
        -> Interpreters (i :+: i') m
infixr :*:

-- | Class of interpreter types 'int' which can be used to interpret
-- an instruction type 'i' producing a corresponding 'm a'.
--
-- Instantiated such that 'int' is either:
-- - A single 'Interpreter' on an instruction type 'i'.
-- - A rigid ':*:' composition of 'Interpreter's which must directly correspond
-- to the shape of an ':+:' composed instruction.
--
-- I.E. The interpreter type must compose in the same order and be of the same size
-- as the instruction type.
class InterpretWith int i m where
  interpretWith :: int i m -> i p a -> m a

instance InterpretWith Interpreters i m where
  interpretWith ((Interpreter int) :*: int') i = case i of
    InjL l -> int l
    InjR r -> interpretWith int' r

instance InterpretWith Interpreter i m where
  interpretWith (Interpreter int) i = int i

-- | Interpret a 'Program' using an interpreter on the instruction type.
interpretProgramWith :: (InterpretWith int i m
                        ,Monad m
                        )
                     => int i m
                     -> Program i a
                     -> m a
interpretProgramWith int (Program p) = case p of
  Return a -> return a
  Instr  i -> interpretWith int i
  Bind m f -> interpretProgramWith int m >>= interpretProgramWith int . f

