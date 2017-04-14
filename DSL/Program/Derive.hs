{-# LANGUAGE TemplateHaskell #-}
{-|
Module     : DSL.Program.Derive
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Use TemplateHaskell to derive injection functions for instruction types.

-}
module DSL.Program.Derive
  ( deriveInjections
  ) where

{- WARNING: This code was rushed with no prior understanding of TH and is pretty poor.. -}

import DSL.Program
import DSL.Instruction.Derive

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
validDeclaration (TyConI (DataD dCtx dName dTyVars _dKy dCons dDeriving)) = return (dCtx,dName,dTyVars,dCons)
validDeclaration _ = fail "Non GADT types not supported"



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



    -- Generate a name representing the composite instruction type
    compInstrName <- newName "is"
    compInstrType <- varT compInstrName

    let instrType = conT (_instrSetName instrSetInfo)
        retName   = _instrReturnTyVar $ _instrSetQuantifiers $ instrSetInfo
        retType   = varT retName
        progName  = _instrProgRtVar $ _instrSetQuantifiers $ instrSetInfo

    -- The type signature looks like:
    -- injName :: forall {instrSetQuantifiers ++ instrQuantifiers}
    --          . {instrSetCtx ++ instrCtx}
    --         => {instrParams} -> ProgramUsingIn {instrName} {compInstrName} {_instrReturn}
    injTypeTerminate <- [t| ProgramUsingIn $(instrType) $(return compInstrType) $(retType) |]
    retTypeActuated  <- retType
    let injQuantifiers =  (PlainTV $ _instrReturnTyVar $ _instrSetQuantifiers instrSetInfo)
                         : (PlainTV $ compInstrName)
                         : (_instrQuantifiers instrInfo)

        -- Sometime we need to add an a~instrReturn type to the ctx. It shouldnt
        -- hurt to just always do this, worse case we're adding a~a
        constrainRetCtx = AppT (AppT EqualityT retTypeActuated) (_instrResultType instrInfo)
        injCtx          = (_instrSetCtx instrSetInfo) ++ (_instrCtx instrInfo) ++ [constrainRetCtx]
        instrParams     = _instrParams instrInfo

    instrParams' <- tieBaseProgramTypes instrParams progName compInstrType
    let injType = arrowCompose instrParams' injTypeTerminate
        injSig  = SigD injName $ ForallT injQuantifiers injCtx injType
    return [injSig,injFun]

  where

    -- If the given type is the ProgRtVar name, rename it to 'Program' applied to the given
    -- composite Instr type and actual instr type.
    --
    -- I.E. Maps:
    -- tieBaseProgram 'p Int' 'p' 'is' ~> 'Program is Int'.
    tieBaseProgramType :: Type -- input type
                       -> Name -- base program type var name
                       -> Type -- composite instruction type var name
                       -> Q Type
    tieBaseProgramType ty progName compInstrName = case ty of
        AppT (VarT appName) a
          | appName == progName -> [t| Program $(return compInstrName) $(return a) |]
          | otherwise -> return ty
        _ -> return ty

    -- Map 'tieBaseProgramType' over a list of types.
    tieBaseProgramTypes tys progName compInstrName = mapM (\ty -> tieBaseProgramType ty progName compInstrName) tys

    -- With function arrows ('->') compose a list of 'parameter' types
    --
    -- terminating in some 'return' type.
    -- E.G.
    -- [Int,Char] Bool ~> Int -> Char -> Bool
    -- [] ()           ~> ()
    arrowCompose :: [Type] -- param types
                 -> Type   -- return type
                 -> Type   -- composed type
    arrowCompose []     t = t
    arrowCompose (p:ps) t = AppT (AppT ArrowT p) (arrowCompose ps t)

    -- Fully apply a constructor name to a list of names representing its parameters
    fullyApplyConstructor :: Name -> [Name] -> Q Exp
    fullyApplyConstructor con vs = return $ foldl AppE (ConE con) (map VarE vs)

    -- Map a constructor name to the corresponding injection function name.
    injectionName :: Name -> Q Name
    injectionName n = case nameBase n of
      c:cs -> return $ mkName $ toLower c : cs
      _ -> fail "empty constructor name?"

