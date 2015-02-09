{-# LANGUAGE FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , OverlappingInstances
           , PolyKinds
           , RankNTypes
           , TypeOperators
  #-}
module DSL.Program.Interpreter
  (Interpreter(..)
  ,Interpreters((:*:))

  ,InterpretWith
  ,interpretWith
  ,interpretProgramWith

  ,InterpretUsing
  ,interpretUsing
  ,interpretProgramUsing
  ) where

import DSL.Instruction
import DSL.Program

-- | An Interpreter interprets instructions
-- , producing a result in 'm'.
newtype Interpreter i m = Interpreter {unInterpreter :: forall a. i a -> m a}

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
  interpretWith :: int i m -> i a -> m a

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
interpretProgramWith int p = case p of
  Return a -> return a
  Instr  i -> interpretWith int i
  Bind m f -> interpretProgramWith int m >>= interpretProgramWith int . f


-- | Class of interpreter types 'int' which are defined on some instruction set
-- 'i', and may be used to interpret a compatible instruction set 'i\',
-- producing a corresponding 'm a'.
class InterpretUsing int i i' m where
  interpretUsing :: int i' m -> i a -> m a

-- When the interpreter directly corresponds to the language, delegate
-- reducing complexity.
instance InterpretWith int i m
      => InterpretUsing int i i m where
  interpretUsing = interpretWith

-- Coerce the instruction type to that of the interpreter.
instance (InterpretWith int i' m
         ,i :<= i'
         )
        => InterpretUsing int i i' m where
  interpretUsing int i = interpretWith int $ coerce i

-- | Interpret a 'Program' using a compatible interpreter.
interpretProgramUsing :: (i :<= i'
                         ,InterpretWith int i' m
                         ,Monad m
                         )
                      => int i' m
                      -> Program i a
                      -> m a
interpretProgramUsing int p = interpretProgramWith int $ coerceProgram p

