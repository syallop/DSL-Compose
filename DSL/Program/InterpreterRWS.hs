{-# LANGUAGE
    PolyKinds
  , RankNTypes
  , TypeOperators
  #-}
module DSL.Program.InterpreterRWS where

import DSL.Instruction
import DSL.Program
import DSL.Program.Derive

import Data.Monoid

type InterpreterRWS i m r w s = forall p. InterpreterRWSOn i p m r w s

-- | An InterpreterRWS is a function which takes:
-- - Some read input 'r'
-- - Some input state 's'
-- - An instruction type 'i p a'
-- and produces a result in 'm (a,s,w)'
type InterpreterRWSOn i p m r w s = forall a. r -> s -> i p a -> m (a,s,w)

-- | Compose two InterpreterRWS's on instruction types 'i' and 'j' respectively into an InterpreterRWS
-- which handles the composed Instruction type 'i :+: j'
composeInterpreter :: InterpreterRWSOn i p m r w s -> InterpreterRWSOn j p m r w s -> InterpreterRWSOn (i :+: j) p m r w s
composeInterpreter intI intJ = \r s ij -> case ij of
  InjL i -> intI r s i
  InjR j -> intJ r s j

-- | InfixR synonym for 'composeInterpreter'.
-- Compose two InterpreterRWS's on instruction types 'i' and 'j' respectively into an InterpreterRWS
-- which handles the composed Instruction type 'i :+: j'
(&) :: InterpreterRWSOn i p m r w s -> InterpreterRWSOn j p m r w s -> InterpreterRWSOn (i :+: j) p m r w s
infixr &
(&) = composeInterpreter

-- | Interpret a Program with an identically shaped InterpreterRWS.
interpret :: (Monad m, Monoid w) => InterpreterRWSOn i (Program i) m r w s -> r -> s -> Program i a -> m (a,s,w)
interpret int r s (Program p) = case p of
  Return a -> return (a,s,mempty)
  Instr  i -> int r s i
  Bind m f -> do (m',s',w)    <- interpret int r s m
                 (m'',s'',w') <- interpret int r s' $ f m'
                 return (m'',s'',w `mappend` w')

-- | Interpret a Program with a compatible InterpreterRWS.
--
-- I.E. The InterpreterRWS must cover each of the Instruction types used in the Program
--  , composed in any order. It may also compose other unused Instruction types.
interpretUsing :: (Monad m, Monoid w, i :<= j) => InterpreterRWSOn j (Program i) m r w s -> r -> s -> Program i a -> m (a,s,w)
interpretUsing int r s p = interpret (\r s -> int r s . coerce) r s p

