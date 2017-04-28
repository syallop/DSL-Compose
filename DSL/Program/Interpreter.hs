{-# LANGUAGE
    PolyKinds
  , RankNTypes
  , TypeOperators
  , ScopedTypeVariables
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

  , interpret
  , interpretUsing

  , Reader1 (..)
  , reduceReader1
  , Self (..)
  )
  where

import DSL.Instruction
import DSL.Program
import DSL.Program.InterpreterG hiding (interpret,interpretUsing)
import qualified DSL.Program.InterpreterG as G

-- | Take some 'r' before producing an 'm a'.
newtype Reader1 r m a = Reader1 (r -> m a)

-- | Reduce e 'Reader1' into a 'm a' by supplying an initial 'r'.
reduceReader1 :: r -> Reader1 r m a -> m a
reduceReader1 r (Reader1 rf) = rf r

-- | A reference to the entire composed interpreter may be passed into the
-- context of individual interpreters.
newtype Self p m = Self {_unSelf :: forall b. p b -> m b}

-- | An 'Interpreter' is a function which takes some instruction type 'i p a' and
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
type Interpreter i m = forall p. InterpreterOn i p m

-- | An 'Interpreter' is a function which takes some instruction type 'i p a' and
-- produces some result in 'Reader1 (Self p m)' - I.E. the interpreter is passed
-- a reference to the entire composite interpreter as a reader argument.
--
-- - 'a' is the result type
-- - 'm' is the context of the result type
--
-- When interpreters are composed, 'm' must be the same but the interpreters own
-- context may vary.
--
-- A specific form of 'InterpreterG'/ 'Interpreter' where we expose 'p'.
type InterpreterOn i p m = G.InterpreterG i p m (Reader1 (Self p m))

-- | Interpret a Program with an identically shaped Interpreter.
interpret :: forall m i a. Monad m => InterpreterOn i (Program i) m -> Program i a -> m a
interpret int prog = G.interpret int reduce prog
  where
    reduce :: Reader1 (Self (Program i) m) m b -> m b
    reduce (Reader1 f) = f (Self (interpret int))

-- | Interpret a Program with a compatible Interpreter.
--
-- I.E. The Interpreter must cover each of the Instruction types used in the Program
-- , composed in any order. It may also compose other, unused Instruction types.
interpretUsing :: (Monad m, i :<= j) => InterpreterOn j (Program i) m -> Program i a -> m a
interpretUsing int p = interpret (int . coerce) p

