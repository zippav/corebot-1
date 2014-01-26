{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import CoreSyn
import Data.List
import DynFlags
import GHC
import GHC.Paths (libdir)
import GhcMonad
import DataCon
import HscTypes
import Var
import Outputable
import Name hiding (varName)
import Literal

main = do
  forever $ do
    defaultErrorHandler defaultLogAction $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags [Opt_ImplicitPrelude]
        setSessionDynFlags (updOptLevel 2 (dflags' { ghcLink    = LinkBinary
                                                   , hscTarget  = HscAsm
                                                   , ghcMode    = OneShot
                                                   , optLevel = 2
                                                   }))
        core <- compileToCoreSimplified "C.hs"
        -- liftIO . putStrLn . showppr $ core
        -- liftIO $ putStrLn "------------------"
        liftIO . putStrLn . ppmodule $ core

ppmodule :: CoreModule -> String
ppmodule CoreModule{..} = intercalate ";" (map ppbind cm_binds)

ppbind :: Bind CoreBndr -> String
ppbind (NonRec a e) = ppbinding a e
ppbind (Rec xs) = intercalate ";" (map (uncurry ppbinding) xs)

ppbinding :: CoreBndr -> Expr CoreBndr -> String
ppbinding name exp = ppname name ++ " = " ++ ppexp exp

ppname :: CoreBndr -> String
ppname v = showppr (nameOccName (varName v))

ppexp :: Expr CoreBndr -> String
ppexp (Var i) = ppvar i
ppexp (Lit lit) = pplit lit
ppexp (App f arg) = parensl f ++ " " ++ parensr arg where
  parensl e@Lam{} = wrap (ppexp e)
  parensl e = ppexp e
  parensr e@Lam{} = wrap (ppexp e)
  parensr e@App{} = wrap (ppexp e)
  parensr e = ppexp e
  wrap x = "(" ++ x ++ ")"
ppexp (Lam i e) = "\\" ++ ppvar i ++ "→" ++ ppexp e
ppexp (Case cond i _ty alts) =
  "case " ++ ppexp cond ++ "(" ++ ppname i ++ ")of{" ++ intercalate ";" (map ppalt alts) ++ "}"

ppalt :: Alt CoreBndr -> String
ppalt (con,vars,exp) = ppcon con ++ concat (map (" "++) (map showppr vars)) ++ "→" ++ ppexp exp

ppcon :: AltCon -> String
ppcon (DataAlt c) = ppdatacon c
ppcon (LitAlt l) = pplit l
ppcon DEFAULT = "_"

ppdatacon :: DataCon -> String
ppdatacon = showppr . nameOccName . dataConName

ppvar :: CoreBndr -> String
ppvar v = showppr (nameOccName (varName v))

pplit :: Literal -> String
pplit x = showppr x

showppr :: Outputable a => a -> String
showppr = showSDoc . ppr
