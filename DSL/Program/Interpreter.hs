{-# LANGUAGE PolyKinds
           , RankNTypes
           , TypeOperators
  #-}
module DSL.Program.Interpreter
  ( Interpreter
  , composeInterpreter, (&)
  , interpret
  , interpretUsing
  ) where

import DSL.Instruction
import DSL.Program

-- | An Interpreter is a function which takes an instruction 'i a'
-- and produces some value 'm a'.
type Interpreter i m = forall p a. i p a -> m a

-- | Compose two Interpreters on instruction types 'i' and 'j' respectivly into an Interpreter
-- which handles the composed Instruction type 'i :+: j'.
composeInterpreter :: Interpreter i m -> Interpreter j m -> Interpreter (i :+: j) m
composeInterpreter intI intJ = \ij -> case ij of
  InjL i -> intI i
  InjR j -> intJ j

-- | InfixR synonym for 'composeInterpreter'.
-- Compose two Interpreters on instruction types 'i' and 'j' respectivly into an Interpreter
-- which handles the composed Instruction type 'i :+: j'.
(&) :: Interpreter i m -> Interpreter j m -> Interpreter (i :+: j) m
infixr &
(&) = composeInterpreter

-- | Interpret a Program with an identically shaped Interpreter.
interpret :: Monad m => Interpreter i m -> Program i a -> m a
interpret int (Program p) = case p of
  Return a -> return a
  Instr  i -> int i
  Bind m f -> interpret int m >>= interpret int . f

-- | Interpret a Program with a compatible Interpreter.
--
-- I.E. The Interpreter must cover each of the Instruction types used in the Program
-- , composed in any order. It may also compose other, unused Instruction types.
interpretUsing :: (Monad m, i :<= j) => Interpreter j m -> Program i a -> m a
interpretUsing int i = interpret (int . coerce) i

