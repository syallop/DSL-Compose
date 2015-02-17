{-# LANGUAGE PolyKinds
           , RankNTypes
           , TypeOperators
  #-}
module DSL.Program.Interpreter
  ( Interpreter
  , InterpreterOn

  , composeInterpreter, (&)
  , interpret
  , interpretUsing
  ) where

import DSL.Instruction
import DSL.Program

-- | And Interpreter is a function which takes a function for
-- interpreting base-program values typed 'p b -> m b',
-- then takes an instruction type 'i p a' to produce some result in
-- 'm a'.
--
-- The base program type 'p' is forgotten.
type Interpreter i m = forall p. InterpreterOn i p m


-- | -- | And Interpreter is a function which takes a function for
-- interpreting base-program values typed 'p b -> m b',
-- then takes an instruction type 'i p a' to produce some result in
-- 'm a'.
type InterpreterOn i p m = forall a. (forall b. p b -> m b) -> i p a -> m a

-- | Compose two Interpreters on instruction types 'i' and 'j' respectivly into an Interpreter
-- which handles the composed Instruction type 'i :+: j'.
composeInterpreter :: Interpreter i m -> Interpreter j m -> Interpreter (i :+: j) m
composeInterpreter intI intJ = \intBase ij -> case ij of
  InjL i -> intI intBase i
  InjR j -> intJ intBase j

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
  Instr  i -> int (interpret int) i
  Bind m f -> interpret int m >>= interpret int . f

-- | Interpret a Program with a compatible Interpreter.
--
-- I.E. The Interpreter must cover each of the Instruction types used in the Program
-- , composed in any order. It may also compose other, unused Instruction types.
interpretUsing :: (Monad m, i :<= j) => Interpreter j m -> Program i a -> m a
interpretUsing int i = interpret (\baseInt -> int baseInt . coerce) i

