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

  ,Reducer

  -- * Example interpreter contexts and reductions
  ,Reader1 (..)
  ,reduceReader1
  ,Self (..)

  -- * Mimic the 'Program.Interpreter' API.
  ,InterpreterSelf
  ,InterpreterSelfOn
  ,interpretWithSelf
  ,interpretWithSelfUsing
  )
  where

import DSL.Instruction
import DSL.Program

-- A function which takes some instruction type 'i p a' to produce some result
-- in 'n m a' where
-- 'm' is the resulting type to compile to and 'n' adds functionality to this
-- specific interpreter such that it can be composed with other interpreters
-- with different 'n's but the same 'm'.

-- An Interpreter is a function which takes some instruction type 'i p a' and
-- produces some result in 'n m a' where:
--
-- - 'a' is the result type
-- - 'm' is the context of the result type
-- - 'n' is the context required by the interpreter.
--
-- When interpreters are composed, 'm' must be the same but 'n' is separate.
type InterpreterG i p m n = forall a. i p a -> n m a

-- A Reducer translates from the interpreters context 'n' to the result context
-- 'm'.
type Reducer n m = forall b. n m b -> m b

-- | Interpret a 'Program' with a compatible 'InterpreterG', passing a
-- corresponding reduction function which translates from the interpreters
-- context 'n' to the result context 'n'.
interpret
  :: (Monad m)
  => InterpreterG i (Program i) m n
  -> Reducer n m
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
  -> Reducer n m
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


-- | A reference to the entire composed interpreter may be passed into the
-- context of individual interpreters.
newtype Self p m = Self {_unSelf :: forall b. p b -> m b}

-- | Take some 'r' before producing an 'm a'.
newtype Reader1 r m a = Reader1 (r -> m a)

-- | Reduce e 'Reader1' into a 'm a' by supplying an initial 'r'.
reduceReader1 :: r -> Reader1 r m a -> m a
reduceReader1 r (Reader1 rf) = rf r


-- | An 'InterpreterSelf' is a function which takes some instruction type 'i p a' and
-- produces some result in 'Reader1 (Self p m)' - I.E. the interpreter is passed
-- a reference to the entire composite interpreter as a reader argument.
--
-- - 'a' is the result type
-- - 'm' is the context of the result type
--
-- When interpreters are composed, 'm' must be the same but the interpreters own
-- context may vary.
--
-- A specific form of 'InterpreterG'.
--
-- Mimics the functionality of 'Program.Interpreter Interpreter'
type InterpreterSelf i m = forall p. InterpreterSelfOn i p m

-- | An 'InterpreterSelfOn' is a function which takes some instruction type 'i p a' and
-- produces some result in 'Reader1 (Self p m)' - I.E. the interpreter is passed
-- a reference to the entire composite interpreter as a reader argument.
--
-- - 'a' is the result type
-- - 'm' is the context of the result type
-- - 'p' is the type of the entire program.
-- When interpreters are composed, 'm' must be the same but the interpreters own
-- context may vary.
--
-- A specific form of 'InterpreterG'/ 'InterpreterSelf' where we expose 'p'.
--
-- Mimics the functionality of 'Program.Interpreter InterpreterOn'.
type InterpreterSelfOn i p m = InterpreterG i p m (Reader1 (Self p m))

-- | Interpret a Program with an identically shaped Interpreter.
--
-- Mimics the functionality of 'Program.Interpreter interpretWith'.
interpretWithSelf :: forall m i a. Monad m => InterpreterSelfOn i (Program i) m -> Program i a -> m a
interpretWithSelf int prog = interpret int reduce prog
  where
    reduce :: Reader1 (Self (Program i) m) m b -> m b
    reduce (Reader1 f) = f (Self (interpretWithSelf int))

-- | Interpret a Program with a compatible Interpreter.
--
-- I.E. The Interpreter must cover each of the Instruction types used in the Program
-- , composed in any order. It may also compose other, unused Instruction types.
--
-- Mimics the functionlity of 'Program.Interpreter interpretUsing'.
interpretWithSelfUsing :: (Monad m, i :<= j) => InterpreterSelfOn j (Program i) m -> Program i a -> m a
interpretWithSelfUsing int p = interpretWithSelf (int . coerce) p

