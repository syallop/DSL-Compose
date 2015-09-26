{-# LANGUAGE
    PolyKinds
  , RankNTypes
  , TypeOperators
  #-}
{-|
Module     : DSL.Program.Interpreter
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Explicitly compose 'Interpreter's of instruction types and use them to interpret
identical or compatiblly structured 'Program's.
 -}
module DSL.Program.Interpreter
  ( Interpreter
  , InterpreterOn

  , composeInterpreter, (&)
  , interpret
  , interpretUsing
  ) where

import DSL.Instruction
import DSL.Program

-- | And Interpreter is a function which takes an instruction type 'i p a'
-- to produce some result in 'm a'.
--
-- The base program type 'p' is forgotten.
type Interpreter i m = forall p. InterpreterOn i p m


-- | An Interpreter is a function which takes an instruction type 'i p a'
-- to produce some result in 'm a'.
type InterpreterOn i p m = forall a. i p a -> m a

-- | Compose two Interpreters on instruction types 'i' and 'j' respectivly into an Interpreter
-- which handles the composed Instruction type 'i :+: j'.
composeInterpreter :: InterpreterOn i p m -> InterpreterOn j p m -> InterpreterOn (i :+: j) p m
composeInterpreter intI intJ = \ij -> case ij of
  InjL i -> intI i
  InjR j -> intJ j

-- | InfixR synonym for 'composeInterpreter'.
-- Compose two Interpreters on instruction types 'i' and 'j' respectivly into an Interpreter
-- which handles the composed Instruction type 'i :+: j'.
(&) :: InterpreterOn i p m -> InterpreterOn j p m -> InterpreterOn (i :+: j) p m
infixr &
(&) = composeInterpreter

-- | Interpret a Program with an identically shaped Interpreter.
interpret :: Monad m => InterpreterOn i (Program i) m -> Program i a -> m a
interpret int (Program p) = case p of
  Return a -> return a
  Instr  i -> int i
  Bind m f -> interpret int m >>= interpret int . f

-- | Interpret a Program with a compatible Interpreter.
--
-- I.E. The Interpreter must cover each of the Instruction types used in the Program
-- , composed in any order. It may also compose other, unused Instruction types.
interpretUsing :: (Monad m, i :<= j) => InterpreterOn j (Program i) m -> Program i a -> m a
interpretUsing int i = interpret (int . coerce) i

