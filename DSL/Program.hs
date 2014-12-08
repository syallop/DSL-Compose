{-# LANGUAGE FlexibleContexts
           , GADTs
           , PolyKinds
           , RankNTypes
           , TypeOperators
           , UndecidableInstances
  #-}
module DSL.Program
  ( Program(..)
  , MonadInstr(..)

  , inject
  , Compile(compile)
  , ProgramUsing
  ) where

import DSL.Instruction

import Control.Applicative
import Control.Monad

-- | Concrete representation of a monadic sequence of
-- instructions.
data MonadInstr i m a where

  -- | Insert an instruction into the context.
  Instr  :: i m a -> MonadInstr i m a

  -- | Monad 'return'.
  Return :: a -> MonadInstr i m a

  -- | Monad 'bind' / '>>='.
  Bind :: m a -> (a -> m b) -> MonadInstr i m b

-- | A Program is a monadic sequence of instructions 'i' which can
-- be interpreted to produce some return value 'a'.
newtype Program i a = Program (MonadInstr i (Program i) a)

instance Monad (Program i) where
  return   = Program . Return
  ma >>= f = Program $ Bind ma f

instance Applicative (Program i) where
  pure  = return
  (<*>) = ap

instance Functor (Program i) where
  fmap = liftM

-- | Inject an instruction into a containing that instruction type.
inject :: (i :<- i') => i (Program i') a -> Program i' a
inject = Program . Instr . inj

-- | Type of 'Program's that may use an instruction type 'i' as part
-- of some composed instruction set.
type ProgramUsing i a = forall i'. (i :<- i') => Program i' a


-- | Class of type's parameterised over 'a', we can canonically compile
-- to produce an 'a'.
class Compile t where
  compile :: t a -> a

-- Instruction compositions can be compiled when each composed instruction type
-- can be compiled.
instance (Compile (i p)
         ,Compile (j p)
         )
       => Compile ((i :+: j) p) where
  compile (InjL l) = compile l

instance (Compile m
         ,Compile (i m)
         )
        => Compile (MonadInstr i m) where
  compile mi = case mi of
    Instr i  -> compile i
    Return a -> a
    Bind m f -> compile $ f $ compile m

instance Compile (i (Program i))
      => Compile (Program i) where
  compile (Program p) = compile p

