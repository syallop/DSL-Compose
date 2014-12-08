{-# LANGUAGE GADTs #-}
module DSL.Program
  ( Program(..)
  , MonadInstr(..)
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

