{-# LANGUAGE TemplateHaskell #-}
{-|
Module     : DSL.Program.Derive
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Use TemplateHaskell to derive injection functions for instruction types.

Note: Currently does not work for instruction types whose constructors use the 'p' typevar.

I.E. NOT instructions like:
data Instr p a where
  Instr :: p Char -> Instr p ()
-}
module DSL.Program.Derive
  ( deriveInjections
  ) where

import DSL.Program

import Data.Char

import Control.Monad.IO.Class
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Given a name which refers to an instruction set like GADT
-- , derive corresponding injection functions.
--
-- For example:
--
-- Given:
--
-- @
--  data InstrSet (p :: * -> *) (a :: *) where
--    Instr1 :: Int -> Char -> InstrSet p Bool
--    Instr2 :: InstrSet p ()
-- @
--
-- Calling:
--
-- @
--   $(deriveInjections ''InstrSet)
-- @
--
-- Will generate the injection functions:
--
-- @
-- instr1 :: Int -> Char -> ProgramUsing InstrSet Bool
-- instr1 i c = inject $ Instr1 i c
--
-- instr2 :: ProgramUsing InstrSet ()
-- instr2 = inject Instr2
-- @
--
-- Note: 'RankNTypes' is required at the splice site.
deriveInjections :: Name -> Q [Dec]
deriveInjections tName = do
    info <- reify tName
    (dCtx,dName,dTyVars,dCons) <- validDeclaration info

    instrSetInfo <- extractInstrSetInfo dCtx dName dTyVars
    instrInfos   <- extractInstrInfos dCons

    concatMapM (generateInjectionFunction instrSetInfo) instrInfos
  where
    concatMapM f xs = liftM concat (mapM f xs)


-- Is the sort of thing we've been given something we can attempt to derive
-- injection functions from?
--
-- => It must be a GADT.
validDeclaration :: Info -> Q (Cxt,Name,[TyVarBndr],[Con])
validDeclaration (TyConI (DataD dCtx dName dTyVars dCons dDeriving)) = return (dCtx,dName,dTyVars,dCons)
validDeclaration _ = fail "Non GADT types not supported"

-- | Two type variables which all of our instruction types must have.
data InstrTyVars = InstrTyVars
  {_instrProgRtVar   :: Name -- ^ Name of composite program type.
  ,_instrReturnTyVar :: Name -- ^ Name of return type.
  }

-- | Represents information about an instruction set like type 
-- , but NOT any contained instructions/ constructors.
data InstrSetInfo = InstrSetInfo
  {_instrSetCtx         :: Cxt         -- ^ data {cxt} => InstrSet p a where
  ,_instrSetName        :: Name        -- ^ data cxt => {InstrSet} p a where
  ,_instrSetQuantifiers :: InstrTyVars -- ^ data cxt => InstrSet {p a} where
  }

-- | Given an instruction sets context, type-name, and type variables
-- , try and extract a vali InstrSetInfo in Q.
extractInstrSetInfo :: Cxt -> Name -> [TyVarBndr] -> Q InstrSetInfo
extractInstrSetInfo cxt name quantifiers = do
  instrQuantifiers <- extractInstrTyVars quantifiers
  return $ InstrSetInfo cxt name instrQuantifiers

-- | Represents information held on a single instruction of an instruction set.
--
-- E.G. It is a GADT constructor of the form:
-- Name :: forall TVARS. CTX => TPARAMS -> InstrSetName INSTRSETTVARS
data InstrInfo = InstrInfo
  {_instrName        :: Name        -- {Name} :: forall TVARS. CXT => TPARAMS -> InstrSetName INSTRSETTVARS
  ,_instrQuantifiers :: [TyVarBndr] -- Name :: forall {TVARS}. CXT => TPARAMS -> InstrSetName INSTRSETTVARS
  ,_instrCtx         :: Cxt         -- Name :: forall TVARS. {CXT} => TPARAMS -> InstrSetName INSTRSETTVARS
  ,_instrParams      :: [Type]      -- Name :: forall TVARS. CXT => {TPARAMS} -> InstrSetName INSTRSETTVARS
  }

-- | Extract all of a list of GADT constructors
-- into a list of individual instructions.
--
-- Fails if any constructor fails to convert.
extractInstrInfos :: [Con] -> Q [InstrInfo]
extractInstrInfos = mapM extractInstrInfo

-- | Extract a GADT constructor into an instruction.
--
-- Fails if the constructor is not GADT-like.
extractInstrInfo :: Con -> Q InstrInfo
extractInstrInfo c = case c of
  (ForallC instrQuantifiers instrCtx (NormalC instrName instrParams))
    -> return $ InstrInfo instrName instrQuantifiers instrCtx (map snd instrParams)

  (ForallC _ _ _)
    -> fail "Records and infix instructions not supported"

  _ -> fail "Non 'forall' quantified instructions not supported. Use a GADT?"

-- | Given a list of TypeVariables used in an instructionSet-like declaration
-- , attempt to extract a InstrTyVars (containing program and return types)
--
-- Succeeds only with two type variables kinded:
-- (prog :: * -> *) (ret :: *)
extractInstrTyVars :: [TyVarBndr] -> Q InstrTyVars
extractInstrTyVars [prog,ret]
    | not $ validProgramTyVar = fail "Program TyVar (first type) must have kind (* -> *)"
    | not $ validReturnTyVar  = fail "Return TyVar (second type) must have kind *"
    | otherwise = return $ InstrTyVars (tyVarBndrName prog) (tyVarBndrName ret)
  where
    validReturnTyVar = case ret of
      PlainTV name        -> True
      KindedTV name StarT -> True
      _                   -> False

    validProgramTyVar = case prog of
      KindedTV name (AppT (AppT ArrowT StarT) StarT)
        -> True

      _ -> False

    tyVarBndrName t = case t of
      PlainTV  n   -> n
      KindedTV n _ -> n
extractInstrTyVars _ = fail "instruction set must be kinded '(* -> *) -> * -> *'"

-- | Generate the function and type signature of an injection function
-- for a instruction within some instruction set.
generateInjectionFunction :: InstrSetInfo -- ^ Containing instruction set
                          -> InstrInfo    -- ^ Specific member instruction
                          -> Q [Dec]      -- ^ Generated function + signature
generateInjectionFunction instrSetInfo instrInfo = do

    -- Generate a parameter every one required by the instruction
    params  <- mapM (\_ -> newName "p") (_instrParams instrInfo)

    -- Map the constructor name to the injection function name
    injName <- injectionName (_instrName instrInfo)

    -- The injection function looks like:
    -- injName {injParams} = inject $ (CONSTRUCTOR {injParams})
    injBody <- [e| inject ( $(fullyApplyConstructor (_instrName instrInfo) params) ) |]
    let injFun = FunD injName [Clause (map VarP params) (NormalB injBody) []]

    let instrType = conT (_instrSetName instrSetInfo)
        retName   = _instrReturnTyVar $ _instrSetQuantifiers $ instrSetInfo
        retType   = varT retName

    -- The type signature looks like:
    -- injName :: forall {instrSetQuantifiers ++ instrQuantifiers}
    --          . {instrSetCtx ++ instrCtx}
    --         => {instrParams} -> ProgramUsing {instrName} {_instrReturn}
    injTypeTerminate <- [t| ProgramUsing $(instrType) $(retType) |]
    let injQuantifiers = (PlainTV $ _instrReturnTyVar $ _instrSetQuantifiers instrSetInfo) : (_instrQuantifiers instrInfo)
        injCtx         = (_instrSetCtx instrSetInfo) ++ (_instrCtx instrInfo)
        injType        = arrowCompose (_instrParams instrInfo) injTypeTerminate
        injSig         = SigD injName $ ForallT injQuantifiers injCtx injType
    return [injSig,injFun]

  where

    -- With function arrows ('->') compose a list of 'parameter' types
    --
    -- terminating in some 'return' type.
    -- E.G.
    -- [Int,Char] Bool ~> Int -> Char -> Bool
    -- [] ()           ~> ()
    arrowCompose :: [Type] -- param types
                 -> Type   -- return type
                 -> Type   -- composed type
    arrowCompose []    t = t
    arrowCompose (p:ps) t = AppT (AppT ArrowT p) (arrowCompose ps t)

    -- Fully apply a constructor name to a list of names representing its parameters
    fullyApplyConstructor :: Name -> [Name] -> Q Exp
    fullyApplyConstructor con vs = return $ foldl AppE (ConE con) (map VarE vs)

    -- Map a constructor name to the corresponding injection function name.
    injectionName :: Name -> Q Name
    injectionName n = case nameBase n of
      c:cs -> return $ mkName $ toLower c : cs
      _ -> fail "empty constructor name?"

