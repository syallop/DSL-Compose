{-# LANGUAGE
    PolyKinds
  , RankNTypes
  , TypeOperators
  #-}
{-|
Module     : DSL.Program.InterpreterS
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Similar API to 'DSL.Program.Interpret', where the 'InterpreterS' type differs from
'Interpreter' by threading some 'state' type in and out of interpreters.
 -}
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

type InterpreterS i m s = forall p. InterpreterSOn i p m s

-- | And InterpreterS is a function which takes:
-- - Some input state 's'
-- - A function for interpreting base-programs (which is passed the state value)
--   typed 's -> p b -> m (b,s)',
-- - And takes an instruction type 'i p a' to produce some result in
--   'm (a,s)' along with a resulting state.
type InterpreterSOn i p m s = forall a. s -> (forall b. s -> p b -> m (b,s)) -> i p a -> m (a,s)

-- | Compose two InterpreterS's on instruction types 'i' and 'j' respectively into an InterpreterS
-- which handles the composed Instruction type 'i :+: j'.
composeInterpreter :: InterpreterSOn i p m s -> InterpreterSOn j p m s -> InterpreterSOn (i :+: j) p m s
composeInterpreter intI intJ = \s intBase ij -> case ij of
  InjL i -> intI s intBase i
  InjR j -> intJ s intBase j

-- | InfixR synonym for 'composeInterpreter'.
-- Compose two InterpreterS's on instruction types 'i' and 'j' respectively into an InterpreterS
-- which handles the composed Instruction type 'i :+: j'.
(&) :: InterpreterSOn i p m s -> InterpreterSOn j p m s -> InterpreterSOn (i :+: j) p m s
infixr &
(&) = composeInterpreter

-- | Interpret a Program with an identically shaped InterpreterS.
interpret :: Monad m => InterpreterSOn i (Program i) m s -> s -> Program i a -> m (a,s)
interpret int s (Program p) = case p of
  Return a -> return (a,s)
  Instr  i -> int s (interpret int) i
  Bind m f -> do (m',s') <- interpret int s m
                 interpret int s' $ f m'

-- | Interpret a Program with a compatible InterpreterS.
--
-- I.E. The InterpreterS must cover each of the Instruction types used in the Program
--  , composed in any order. It may also compose other, unused Instruction type.
interpretUsing :: (Monad m, i :<= j) => InterpreterSOn j (Program i) m s -> s -> Program i a -> m (a,s)
interpretUsing int s p = interpret (\s baseInt -> int s baseInt . coerce) s p

