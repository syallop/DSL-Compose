{-# LANGUAGE TemplateHaskell #-}
{-|
Module     : DSL.Instruction.Derive
Copyright  : (c) Samuel A. Yallop, 2015
Maintainer : syallop@gmail.com
Stability  : experimental

Use TemplateHaskell to derive boilerplate for instruction types.

-}
module DSL.Instruction.Derive
  ( InstrTyVars (),_instrProgRtVar,_instrReturnTyVar
  , extractInstrTyVars

  , InstrSetInfo (), _instrSetCtx,_instrSetName,_instrSetQuantifiers
  , extractInstrSetInfo

  , InstrInfo (),_instrName,_instrQuantifiers,_instrCtx,_instrParams,_instrResultType
  , extractInstrInfos
  , extractInstrInfo
  ) where

import Language.Haskell.TH

-- | Two type variables which all of our instruction types must have.
data InstrTyVars = InstrTyVars
  {_instrProgRtVar   :: Name -- ^ Name of composite program type.
  ,_instrReturnTyVar :: Name -- ^ Name of return type.
  }

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
      PlainTV  _name       -> True
      KindedTV _name StarT -> True
      _                   -> False

    validProgramTyVar = case prog of
      KindedTV _name (AppT (AppT ArrowT StarT) StarT)
        -> True

      _ -> False

    tyVarBndrName t = case t of
      PlainTV  n   -> n
      KindedTV n _ -> n
extractInstrTyVars _ = fail "instruction set must be kinded '(* -> *) -> * -> *'"


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
extractInstrSetInfo ctx name quantifiers = do
  instrQuantifiers <- extractInstrTyVars quantifiers
  return $ InstrSetInfo ctx name instrQuantifiers


-- | Represents information held on a single instruction of an instruction set.
--
-- E.G. It is a GADT constructor of the form:
-- Name :: forall TVARS. CTX => TPARAMS -> InstrSetName INSTRSETTVARS
data InstrInfo = InstrInfo
  {_instrName        :: Name        -- {Name} :: forall TVARS. CXT => TPARAMS -> InstrSetName INSTRSETTVARS
  ,_instrQuantifiers :: [TyVarBndr] -- Name :: forall {TVARS}. CXT => TPARAMS -> InstrSetName INSTRSETTVARS
  ,_instrCtx         :: Cxt         -- Name :: forall TVARS. {CXT} => TPARAMS -> InstrSetName INSTRSETTVARS
  ,_instrParams      :: [Type]      -- Name :: forall TVARS. CXT => {TPARAMS} -> InstrSetName INSTRSETTVARS
  ,_instrResultType  :: Type        -- Name :: forall TVARS. CTX => TPARAMS -> InstrSetName p {RESULTTYPE}
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
  (ForallC instrQuantifiers instrCtx constructor)
    -> case constructor of
         GadtC [instrName] instrParams (AppT (AppT (ConT _instrName) (VarT _pname)) instrResultTy)
           -> return $ InstrInfo instrName instrQuantifiers instrCtx (map snd instrParams) instrResultTy

         _ -> fail "Records and infix instructions not supported"

  _ -> fail "Non 'forall' quantified instructions not supported. Use a GADT?"

