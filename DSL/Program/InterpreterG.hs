{-# LANGUAGE
    GADTs
  , PolyKinds
  , FlexibleContexts
  , RankNTypes
  , TypeOperators
  , ScopedTypeVariables
  #-}
{-|
Module     : DSL.Program.InterpreterG
Copyright  : (c) Samuel A. Yallop, 2017
Maintainer : syallop@gmail.com
Stability  : experimental

A more general API than 'DSL.Program.Interpreter' allowing interpreters to run
in their own separate contexts and still be composed.
 -}
module DSL.Program.InterpreterG
  (InterpreterG

  ,interpret
  ,interpretUsing
  ,compose, (&)
  )
  where

import DSL.Instruction
import DSL.Program

import Control.Monad

-- A function which takes some instruction type 'i p a' to produce some result
-- in 'n m a' where
-- 'm' is the resulting type to compile to and 'n' adds functionality to this
-- specific interpreter such that it can be composed with other interpreters
-- with different 'n's but the same 'm'.

-- An Interpreter is a function which takes some instruction type 'i p a' to and
-- produces some result in 'n m a' where:
--
-- - 'a' is the result type
-- - 'm' is the context of the result type
-- - 'n' is the context required by the interpreter.
--
-- When interpreters are composed, 'm' must be the same but 'n' is separate.
type InterpreterG i p m n = forall a. i p a -> n m a


-- | Interpret a 'Program' with a compatible 'InterpreterG', passing a
-- corresponding reduction function which translates from the interpreters
-- context 'n' to the result context 'n'.
interpret
  :: (Monad m)
  => InterpreterG i (Program i) m n
  -> (forall b. n m b -> m b)       -- ^ A reduction function from the interpreters context to the results context
  -> Program i a
  -> m a
interpret int reduce (Program p) = case p of
  Return a
    -> return a

  Instr i
    -> reduce $ int i

  Bind m f
    -> interpret int reduce m >>= interpret int reduce . f 

-- | Interpret a Program as with 'interpret' but using a compatible rather than
-- an exact 'InterpreterG'.
--
-- I.E. The InterpreterG must cover each of the Instruction types used in the
-- Program, composed in any order. It may also compose other, unused Instruction
-- types.
interpretUsing 
  :: (Monad m
     ,i :<= j
     )
  => InterpreterG j (Program i) m n
  -> (forall b. n m b -> m b)       -- ^ A reduction function from the interpreters context to the results context
  -> Program i a
  -> m a
interpretUsing int reduce p = interpret (int . coerce) reduce p

-- | Compose two 'InterpreterG's on instruction types 'i' and 'j' respectivly
-- into an 'InterpreterG' which handles the composed Instruction type 'i :+: j'.
--
-- Both 'InterpreterG's must have the same result context 'm' but their own
-- context 'ni' and 'nj' may differ as they will compose to 'ni :+: nj'
-- similarly to the instruction type.
compose
  :: InterpreterG  i        p m  ni
  -> InterpreterG        j  p m         nj
  -> InterpreterG (i :+: j) p m (ni :+: nj)
compose intI intJ = \ij -> case ij of
  InjL i
    -> InjL $ intI i

  InjR j
    -> InjR $ intJ j

-- | InfixR synonym for 'compose'.
(&) :: InterpreterG  i        p m  ni
    -> InterpreterG        j  p m         nj
    -> InterpreterG (i :+: j) p m (ni :+: nj)
infixr &
(&) = compose

