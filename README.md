# Compose DSL’s and their Interpreters

A quick proof-of-concept of a mechanism for:

1. Composing Domain-Specific-Language’s (a la “Data Types a` la Carte”[1]).
2. With a possible wrapping monadic structure (as in “Simple and Compositional Reification of Monadic Embedded Languages”).
3. Where each DSL may depend upon the composed program type it is contained within.
3. And then interpreting them by explicit composition of interpreters (an extension).

## Example
### Defining DSL’s
We define three separate DSL’s, one each responsible for arithmetic operations
, boolean operations and IO operations.

```haskell
data ArithOp (prog :: * -> *) ret where
    Add :: Int -> Int -> ArithOp p Int
    Mul :: Int -> Int -> ArithOp p Int

data BoolOp (prog :: * -> *) ret where
    And :: Bool -> Bool -> BoolOp p Bool
    Or  :: Bool -> Bool -> BoolOp p Bool
    Not :: Bool -> BoolOp p Bool

data IOOp (prog :: * -> *) ret where
    GetInt  :: IOOp p Int
    PutInt  :: Int -> IOOp p ()
    GetBool :: IOOp p Bool
    PutBool :: Bool -> IOOp p ()
```

We can then mechanically define functions to inject each instruction into
the type of programs which may use them, by declaring:

```haskell
add :: Int -> Int -> ProgramUsing ArithOp Int
add x y = inject $ Add x y

mul :: Int -> Int -> ProgramUsing ArithOp Int
mul x y = inject $ Add x y

...

getBool :: ProgramUsing IOOp Bool
getBool = inject GetBool

putBool :: Bool -> ProgramUsing IOOp ()
putBool b = inject $ PutBool b

```
etc.
In most cases type signatures can be omitted.

Alternatively “DSL.Program.Derive” can be used to derive these injection functions
using template haskell:

```
$(deriveInjections ‘’ArithOp)

...

$(deriveInjections ‘’IOOp)

```

### Composing DSL’s
Instruction types are composed with ‘:+:’, and ‘Program’ wraps instructions compositions
with the capability to be used as a monadic sequence of instructions.

Therefore:
```haskell
type MyProgram a = Program (ArithOp :+: BoolOp :+: IOOp) a
```
Is the type of monadic programs which may use instructions from either ‘ArithOp’, ‘BoolOp’ or ‘IOOp’.

With this type and the defined injection functions, we can write monadic programs
which implicitly lift and compose the different instruction types like:

```haskell
-- exProgram :: Program (ArithOp :+: BoolOp :+: IOOp) ()
exProgram :: MyProgram ()
exProgram = do
    x <- getInt
    y <- getInt
    z <- getInt

    b <- getBool

    i <- x `add` y
    j <- if b then i `mul` z
              else i `add` z

    putInt j
```

### Writing Interpreters
We can write interpreters for Instruction types like:
```haskell
exArithInterpreter :: Interpreter ArithOp IO
exArithInterpreter = \case
    Add x y -> return $ x + y
    Mul x y -> return $ x + y

...

exIOInterpreter :: Interpreter IOOp IO
exIOInterpreter = \case
    GetInt   -> getLine >>= return . read
    PutInt i -> print i
    ...
```

(Note -XLambdaCase provides the syntax
```haskell
\case
```
which desugars to
```haskell
‘\freshName -> case freshName of’
```
)

### Composing Interpreters
#### Precisely
We can compose interpreters with ‘&’ to produce an interpreter which
can interpret both instruction types.

A function named ‘interpret’ will run a composite
interpreter on a program with an identical composition structure.

E.G. the ‘exProgram’ example could be interpreted:
```haskell
exInterpreter :: Interpreter (ArithOp :+: BoolOp :+: IOOp) IO
exInterpreter = exArithInterpreter
              & exBoolInterpreter
              & exIOInterpreter
main = interpret exInterpreter exProgram
```
Running:
```haskell
>>> 1
>>> 5
>>> 6
>>> True
<<< 36
```

#### Compatibly
Finally, progam can be explicitly interpreted with compatible
(but not structurally identical) intepreter compositions.

‘interpretUsing’ implements this functionality and can therefore be used on
compositions which:
- Are in a different order to the languages.
- Are composed with interpreters the language does not use.

For example, the following could be used with ‘interpretUsing’:

```haskell
-- Different order
exInterpreter1 = exIOInterpreter
               & exArithInterpreter
               & exBoolInterpreter

-- Unused interpreters + different order
exInterpreter2 = exFooInterpreter
               & exBoolInterpreter
               & exIOInterpreter
               & exArithInterpreter
               & exBarInterpreter
```

## Testing
There are no automated tests, however there are three interpreters in Example.hs
testing:
- interpreter1: interpreters composed in the same order as the program
- interpreter2: interpreters composed in a different order as the program
- interpreter3: interpreters composed in a different order, including an
  extraneous interpreter

This file loading in the first place is strong proof that:
- The derivation functionality is working
- Interpreter composition is working

Execute like:
```
> stack ghci
> testExample1
```
Then entering:
```
> 1
> 2
> 3
```
Should produce:

```
Foo
Bar
Baz
```

Derived code can be inspected like `stack build --ghc-options -ddump-splices`

## Citations
1. Wouter Swierstra - “Data Types a` la Carte, Journal of Functional Programming”
2. Josef Svenningsson,Bo Joel Svensson - “Simple and Compositional Reification of Monadic Embedded Languages”

