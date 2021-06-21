{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC
import GHC.Paths (libdir)

import GHC.Driver.Main
import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Driver.Types
import GHC.Core.TyCon
import GHC.CoreToStg.Prep
import GHC.CoreToStg
import GHC.Stg.Syntax

import Control.Monad.Trans

showGhc :: (Outputable a) => a -> String
showGhc = showSDocUnsafe . ppr

banner :: MonadIO m => String -> m ()
banner msg = liftIO $ putStrLn (
  (replicate (fromIntegral n) '=')
  ++
  msg
  ++ 
  (replicate (fromIntegral n) '=')
  )
  where
    n = (76 - length msg) `div` 2

main :: IO ()
main = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted }
  dflags <- getSessionDynFlags

  target <- guessTarget "Example.hs" Nothing
  setTargets [target]
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName "Example"

  pmod <- parseModule modSum      -- ModuleSummary
  tmod <- typecheckModule pmod    -- TypecheckedSource
  dmod <- desugarModule tmod      -- DesugaredModule
  let core = coreModule dmod      -- CoreModule

  (core', _ccs) <- liftIO $ do
      hsc_env <- newHscEnv dflags
      let tycons = mg_tcs core
          data_tycons = filter isDataTyCon tycons
      corePrepPgm hsc_env (mg_module core) (ms_location modSum) (mg_binds core) data_tycons
  let (stg, _cccs) = coreToStg dflags (mg_module core) core'

  liftIO $ banner "Parsed Source"
  liftIO $ putStrLn $ showGhc ( parsedSource pmod )

  liftIO $ banner "Renamed Module"
  liftIO $ putStrLn $ showGhc ( tm_renamed_source tmod )

  liftIO $ banner "Typechecked Module"
  liftIO $ putStrLn $ showGhc ( tm_typechecked_source tmod )

  liftIO $ banner "Typed Toplevel Definitions"
  liftIO $ putStrLn $ showGhc ( modInfoTyThings (moduleInfo tmod) )

  liftIO $ banner "Typed Toplevel Exports"
  liftIO $ putStrLn $ showGhc ( modInfoExports (moduleInfo tmod) )

  liftIO $ banner "Core Module"
  liftIO $ putStrLn $ showGhc ( mg_binds core )

  liftIO $ banner "Core Module (prep'd)"
  liftIO $ putStrLn $ showGhc core'

  liftIO $ banner "STG"
  liftIO $ putStrLn $ showSDocUnsafe $ pprStgTopBindings (initStgPprOpts dflags) stg
