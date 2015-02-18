{-# LANGUAGE PolyKinds
           , RankNTypes
           , TypeOperators
  #-}
module DSL.Program.InterpreterS
  ( InterpreterS
  , InterpreterSOn
  , composeInterpreter, (&)
  , interpret
  , interpretUsing
  ) where

import DSL.Instruction
import DSL.Program
import DSL.Program.Derive

import Language.Haskell.TH

type InterpreterS i m s = forall p a. InterpreterSOn i p m s a

-- | And InterpreterR is a function which takes:
-- - Some 'read' input state 'r'
-- - A function for interpreting base-programs (which is passed the read value)
--   typed 'r -> p b -> m b',
-- - And takes an instruction type 'i p a' to produce some result in
--   'm a'.
type InterpreterSOn i p m s a = s -> (forall b. s -> p b -> m (b,s)) -> i p a -> m (a,s)

-- | Compose two InterpreterR's on instruction types 'i' and 'j' respectivly into an Interpreter
-- which handles the composed Instruction type 'i :+: j'.
composeInterpreter :: InterpreterS i m s -> InterpreterS j m s -> InterpreterS (i :+: j) m s
composeInterpreter intI intJ = \s intBase ij -> case ij of
  InjL i -> intI s intBase i
  InjR j -> intJ s intBase j

-- | InfixR synonym for 'composeInterpreter'.
-- Compose two InterpreterR's on instruction types 'i' and 'j' respectivly into an InterpreterR
-- which handles the composed Instruction type 'i :+: j'.
(&) :: InterpreterS i m s -> InterpreterS j m s -> InterpreterS (i :+: j) m s
infixr &
(&) = composeInterpreter

-- | Interpret a Program with an identically shaped InterpreterR.
interpret :: Monad m => InterpreterS i m s -> s -> Program i a -> m (a,s)
interpret int s (Program p) = case p of
  Return a -> return (a,s)
  Instr  i -> int s (interpret int) i
  Bind m f -> do (m',s') <- interpret int s m
                 interpret int s' $ f m'

-- | Interpret a Program with a compatible InterpreterR.
--
-- I.E. The InterpreterR must cover each of the Instruction types used in the Program
--  , composed in any order. It may also compose other, unused Instruction type.
interpretUsing :: (Monad m, i :<= j) => InterpreterS j m s -> s -> Program i a -> m (a,s)
interpretUsing int s p = interpret (\s baseInt -> int s baseInt . coerce) s p

