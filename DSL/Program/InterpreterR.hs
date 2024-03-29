-- TODO: Do we want interpretUsing to violate this?
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE
    PolyKinds
  , RankNTypes
  , TypeOperators
  #-}
{-|
Module     : DSL.Program.InterpreterR
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Similar API to 'DSL.Program.Interpret', where the 'InterpreterR' type differs from
'Interpreter' by requiring some 'reader' type as input to the interpretation
 -}
module DSL.Program.InterpreterR
  ( InterpreterR
  , InterpreterROn
  , composeInterpreter, (&)
  , interpret
  , interpretUsing
  ) where

import DSL.Instruction
import DSL.Program

-- | And InterpreterR is a function which takes:
-- - Some 'read' input state 'r'
-- - And takes an instruction type 'i p a' to produce some result in
--   'm a'.
--
-- The base program type 'p' is forgotten.
type InterpreterR i m r = forall p. InterpreterROn i p m r

-- | And InterpreterR is a function which takes:
-- - Some 'read' input state 'r'
-- - And takes an instruction type 'i p a' to produce some result in
--   'm a'.
type InterpreterROn i p m r = forall a. r -> (forall b. r -> p b -> m b) -> i p a -> m a

-- | Compose two InterpreterR's on instruction types 'i' and 'j' respectively into an Interpreter
-- which handles the composed Instruction type 'i :+: j'.
composeInterpreter :: InterpreterROn i p m r -> InterpreterROn j p m r -> InterpreterROn (i :+: j) p m r
composeInterpreter intI intJ = \r intBase ij -> case ij of
  InjL i -> intI r intBase i
  InjR j -> intJ r intBase j


-- | InfixR synonym for 'composeInterpreter'.
-- Compose two InterpreterR's on instruction types 'i' and 'j' respectively into an InterpreterR
-- which handles the composed Instruction type 'i :+: j'.
(&) :: InterpreterROn i p m r -> InterpreterROn j p m r -> InterpreterROn (i :+: j) p m r
infixr &
(&) = composeInterpreter

-- | Interpret a Program with an identically shaped InterpreterR.
interpret :: Monad m => InterpreterROn i (Program i) m r -> r -> Program i a -> m a
interpret int r (Program p) = case p of
  Return a -> return a
  Instr  i -> int r (interpret int) i
  Bind m f -> interpret int r m >>= interpret int r . f

-- | Interpret a Program with a compatible InterpreterR.
--
-- I.E. The InterpreterR must cover each of the Instruction types used in the Program
--  , composed in any order. It may also compose other, unused Instruction type.
interpretUsing :: (Monad m, i :<= j) => InterpreterROn j (Program i) m r -> r -> Program i a -> m a
interpretUsing int r p = interpret (\r' baseInt -> int r' baseInt . coerce) r p

