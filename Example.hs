{-# LANGUAGE FlexibleContexts
           , GADTs
           , KindSignatures
           , LambdaCase
           , TemplateHaskell
           , TypeOperators
  #-}
{-# LANGUAGE RankNTypes #-}
module Example where

import DSL.Instruction
import DSL.Program
import DSL.Program.Interpreter

import DSL.Program.Derive
import Language.Haskell.TH

-- | Some arithmetic operations
data ArithOp (p :: * -> *) a where
  Add :: Int -> Int -> ArithOp p Int
  Mul :: Int -> Int -> ArithOp p Int
-- promote arithmetic operations to program instructions
{-add :: Int -> Int -> ProgramUsing ArithOp Int-}
{-add x y = inject $ Add x y-}
-- mul :: Int -> Int -> ProgramUsing ArithOp Int
{-mul x y = inject $ Add x y-}
$(deriveInjections ''ArithOp)

-- | Some IO operations
data IOOp (p :: * -> *) a where
  GetInt :: IOOp p Int
  PutInt :: Int -> IOOp p ()
-- promote io operations to program instructions
--getInt :: ProgramUsing IOOp Int
{-getInt = inject GetInt-}
--putInt :: ProgramUsing IOOp ()
{-putInt x = inject $ PutInt x-}
$(deriveInjections ''IOOp)

-- | Some nonsense operations
data FooOp (p :: * -> *) a where
  Foo :: FooOp p ()
  Bar :: FooOp p ()
  Baz :: FooOp p ()
-- promote nonsense operations to program instructions
{-foo = inject Foo :: ProgramUsing FooOp ()-}
{-bar = inject Bar :: ProgramUsing FooOp ()-}
{-baz = inject Baz :: ProgramUsing FooOp ()-}
$(deriveInjections ''FooOp)

-- | Example Program type, composing three instruction types.
type MyProgram a = Program (ArithOp :+: IOOp :+: FooOp) a

-- | An example 'Program' monadically using three composed instruction types
--
-- Arbitrarily:
-- - Accept three ints x,y,z
-- - Execute foo, bar then bax
-- - put (x+y)*z
exProgram :: MyProgram ()
exProgram = do
  x <- getInt
  y <- getInt
  z <- getInt

  foo
  bar
  baz

  a <- x `add` y
  b <- a `mul` z

  putInt b


-- | An example interpreter for ArithOp's, producing a result in IO
exArithInterpreter :: Interpreter ArithOp IO
exArithInterpreter = \case
  Add x y -> return $ x + y
  Mul x y -> return $ x * y

-- | An example interpreter for IOOp's, producing a result in IO.
exIOInterpreter :: Interpreter IOOp IO
exIOInterpreter = \case
  GetInt   -> getLine >>= return . read
  PutInt i -> print i

-- | An example interpreter for FooOp's, producing a result in IO.
exFooInterpreter :: Interpreter FooOp IO
exFooInterpreter = \case
  Foo -> putStrLn "Foo"
  Bar -> putStrLn "Bar"
  Baz -> putStrLn "Baz"


-- | A valid composite interpreter for 'exProgram', composing interpreters in
-- the same order as the program instruction composition order.
interpreter1 = exArithInterpreter & exIOInterpreter & exFooInterpreter
testExample1 = interpretUsing interpreter1 exProgram

-- | A valid composite interpreter for 'exProgram', composing interpreters in
-- a different order as the program instruction composition order.
interpreter2 = exFooInterpreter & exArithInterpreter & exIOInterpreter
testExample2 = interpretUsing interpreter2 exProgram

-- | An empty instruction type
data EmptyInst (p :: * -> *) a

-- | An empty interpreter
exEmptyInterpreter :: Interpreter EmptyInst IO
exEmptyInterpreter = const undefined

-- | A valid composite interpreter for 'exProgram', composing interpreters in
-- a different order as the program composition order, and including an
-- extraneous interpreter.
interpreter3 = exArithInterpreter
             & exEmptyInterpreter
             & exFooInterpreter
             & exIOInterpreter
testExample3 = interpretUsing interpreter3 exProgram

