-- TODO: Deriviations would ideally not generate unused quantifiers.
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-# LANGUAGE FlexibleContexts
           , GADTs
           , ImpredicativeTypes
           , KindSignatures
           , LambdaCase
           , RankNTypes
           , TemplateHaskell
           , TypeOperators
           , ScopedTypeVariables
  #-}
module Example where

import Data.Kind

import DSL.Instruction
import DSL.Program
import DSL.Program.InterpreterG

import DSL.Program.Derive

-- | Some arithmetic operations
data ArithOp (p :: Type -> Type) a where
  Add :: Int -> Int -> ArithOp p Int
  Mul :: Int -> Int -> ArithOp p Int
-- promote arithmetic operations to program instructions
{-add :: Int -> Int -> ProgramUsing ArithOp Int-}
{-add x y = inject $ Add x y-}
-- mul :: Int -> Int -> ProgramUsing ArithOp Int
{-mul x y = inject $ Add x y-}
$(deriveInjections ''ArithOp)
instance MapProgram ArithOp where
  mapProgram _f = \case
    Add x y -> Add x y
    Mul x y -> Mul x y


-- | Some IO operations
data IOOp (p :: Type -> Type) a where
  GetInt :: IOOp p Int
  PutInt :: Int -> IOOp p ()
-- promote io operations to program instructions
--getInt :: ProgramUsing IOOp Int
{-getInt = inject GetInt-}
--putInt :: ProgramUsing IOOp ()
{-putInt x = inject $ PutInt x-}
$(deriveInjections ''IOOp)
instance MapProgram IOOp where
  mapProgram _ = \case
    GetInt   -> GetInt
    PutInt x -> PutInt x

-- | Some nonsense operations
data FooOp (p :: Type -> Type) a where
  Foo :: FooOp p ()
  Bar :: FooOp p ()
  Baz :: FooOp p ()
-- promote nonsense operations to program instructions
{-foo = inject Foo :: ProgramUsing FooOp ()-}
{-bar = inject Bar :: ProgramUsing FooOp ()-}
{-baz = inject Baz :: ProgramUsing FooOp ()-}
$(deriveInjections ''FooOp)
instance MapProgram FooOp where
  mapProgram _ = \case
    Foo -> Foo
    Bar -> Bar
    Baz -> Baz

-- | Example Program type, composing three instruction types.
type MyProgram a = Program (ArithOp :+: IOOp :+: FooOp) a

-- | An example program based only upon the IOOp instruction type.
-- Requests and returns a tuple of three Ints.
getThreeInts :: Program IOOp (Int,Int,Int)
getThreeInts = do
  x <- getInt
  y <- getInt
  z <- getInt
  return (x,y,z)

-- | An example 'Program' monadically using three composed instruction types
--
-- Arbitrarily:
-- - Accept three ints x,y,z
-- - Execute foo, bar then bax
-- - put (x+y)*z
exProgram :: MyProgram ()
exProgram = do
  (x,y,z) <- embed getThreeInts

  foo
  bar
  baz

  a <- x `add` y
  b <- a `mul` z

  putInt b

-- | An example interpreter for ArithOp's, producing a result in IO
exArithInterpreter :: InterpreterSelf ArithOp IO
exArithInterpreter i = Reader1 $ \_self -> case i of
  Add x y -> return $ x + y
  Mul x y -> return $ x * y
arithReducer :: Reducer (Reader1 (Self (Program ArithOp) IO)) IO
arithReducer (Reader1 f) = f $ Self $ interpret exArithInterpreter arithReducer

-- | An example interpreter for IOOp's, producing a result in IO.
exIOInterpreter :: InterpreterSelf IOOp IO
exIOInterpreter inst = Reader1 $ \_self -> case inst of
  GetInt   -> getLine >>= return . read
  PutInt i -> print i
ioReducer :: Reducer (Reader1 (Self (Program IOOp) IO)) IO
ioReducer (Reader1 f) = f $ Self $ interpret exIOInterpreter ioReducer


-- | An example interpreter for FooOp's, producing a result in IO.
{-exFooInterpreter :: InterpreterG FooOp p IO (Reader1 (Self p IO))-}
exFooInterpreter :: InterpreterSelf FooOp IO
exFooInterpreter i = Reader1 $ \_self -> case i of
  Foo -> putStrLn "Foo"
  Bar -> putStrLn "Bar"
  Baz -> putStrLn "Baz"
fooReducer :: Reducer (Reader1 (Self (Program FooOp) IO)) IO
fooReducer (Reader1 f) = f $ Self $ interpret exFooInterpreter fooReducer


-- | A valid composite interpreter for 'exProgram', composing interpreters in
-- the same order as the program instruction composition order.

interpreter1
  :: InterpreterG (ArithOp :+: IOOp :+: FooOp)
                  (Program (ArithOp :+: IOOp :+: FooOp))
                  IO
                  (    Reader1 (Self (Program (ArithOp :+: IOOp :+: FooOp)) IO)
                   :+: Reader1 (Self (Program (ArithOp :+: IOOp :+: FooOp)) IO)
                   :+: Reader1 (Self (Program (ArithOp :+: IOOp :+: FooOp)) IO)
                  )
interpreter1 = exArithInterpreter & (exIOInterpreter & exFooInterpreter)

testExample1 :: IO ()
testExample1 = interpretUsing interpreter1 reducer exProgram
  where
    reducer :: (is~(ArithOp :+: IOOp :+: FooOp)
               ,m~IO
               )
            =>  (   Reader1 (Self (Program is) m)
                 :+:Reader1 (Self (Program is) m)
                 :+:Reader1 (Self (Program is) m)
                ) IO b
            -> IO b
    reducer rs = do
      case rs of
        InjL (Reader1 f)
          -> f self

        InjR (InjL (Reader1 f))
          -> f self

        InjR (InjR (Reader1 f))
          -> f self

    self :: Self (Program (ArithOp :+: IOOp :+: FooOp)) IO
    self = Self $ interpret interpreter1 reducer


-- | A valid composite interpreter for 'exProgram', composing interpreters in
-- a different order as the program instruction composition order.
interpreter2
  :: InterpreterG (FooOp :+: ArithOp :+: IOOp)
                  (Program is)
                  IO
                  (  Reader1 (Self (Program is) IO)
                  :+:Reader1 (Self (Program is) IO)
                  :+:Reader1 (Self (Program is) IO)
                  )
interpreter2 = exFooInterpreter & exArithInterpreter & exIOInterpreter

testExample2 :: IO ()
testExample2 = interpretUsing interpreter2 reducer exProgram
  where
    reducer :: (is~(ArithOp :+: IOOp :+: FooOp)
               ,m~IO
               )
            => (  Reader1 (Self (Program is) m)
               :+:Reader1 (Self (Program is) m)
               :+:Reader1 (Self (Program is) m)
               ) IO b
            -> IO b
    reducer rs = case rs of
      InjL (Reader1 f)
        -> f self

      InjR (InjL (Reader1 f))
        -> f self

      InjR (InjR (Reader1 f))
        -> f self
      where
        self :: Self (Program (ArithOp :+: IOOp :+: FooOp)) IO
        self = Self $ interpretUsing interpreter2 reducer

-- | An empty instruction type
data EmptyInst (p :: Type -> Type) a

-- | An empty interpreter
exEmptyInterpreter :: InterpreterSelf EmptyInst IO
exEmptyInterpreter _i = Reader1 $ \_self -> undefined

-- | A valid composite interpreter for 'exProgram', composing interpreters in
-- a different order as the program composition order, and including an
-- extraneous interpreter.
interpreter3
  :: InterpreterG (ArithOp :+: EmptyInst :+: FooOp :+: IOOp)
                  (Program is)
                  IO
                  (    Reader1 (Self (Program is) IO)
                   :+: Reader1 (Self (Program is) IO)
                   :+: Reader1 (Self (Program is) IO)
                   :+: Reader1 (Self (Program is) IO)
                  )
interpreter3 = exArithInterpreter
             & exEmptyInterpreter
             & exFooInterpreter
             & exIOInterpreter

testExample3 :: IO ()
testExample3 = interpretUsing interpreter3 reducer exProgram
  where
    reducer :: (is~(ArithOp :+: IOOp :+: FooOp)
               ,m~IO
               )
            => (  Reader1 (Self (Program is) m)
               :+:Reader1 (Self (Program is) m)
               :+:Reader1 (Self (Program is) m)
               :+:Reader1 (Self (Program is) m)
               ) IO b
            -> IO b
    reducer rs = case rs of
      InjL (Reader1 f)
        -> f self

      InjR (InjL (Reader1 f))
        -> f self

      InjR (InjR (InjL (Reader1 f)))
        -> f self

      InjR (InjR (InjR (Reader1 f)))
        -> f self
      where
        self :: Self (Program (ArithOp :+: IOOp :+: FooOp)) IO
        self = Self $ interpretUsing interpreter3 reducer

