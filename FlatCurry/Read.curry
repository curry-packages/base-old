------------------------------------------------------------------------------
--- This library defines operations to read a FlatCurry programs or interfaces
--- together with all its imported modules in the current load path.
---
--- @author Michael Hanus, Bjoern Peemoeller
--- @version September 2016
--- @category meta
------------------------------------------------------------------------------

module FlatCurry.Read
  ( readFlatCurryWithImports
  , readFlatCurryWithImportsInPath
  , readFlatCurryIntWithImports
  , readFlatCurryIntWithImportsInPath
  ) where

import Directory    (getModificationTime)
import Distribution ( getLoadPathForModule, lookupModuleSourceInLoadPath
                    , FrontendTarget (FCY), callFrontendWithParams
                    , defaultParams, setQuiet, setFullPath
                    )
import FileGoodies  (baseName, lookupFileInPath, stripSuffix)
import FilePath     (normalise)

import FlatCurry.Types
import FlatCurry.Files

--- Reads a FlatCurry program together with all its imported modules.
--- The argument is the name of the main module,
--- possibly with a directory prefix.
readFlatCurryWithImports :: String -> IO [Prog]
readFlatCurryWithImports modname = do
  loadpath <- getLoadPathForModule modname
  readFlatCurryFileWithImports False loadpath (baseName modname) [".fcy"]

--- Reads a FlatCurry program together with all its imported modules in a given
--- load path.
--- The arguments are a load path and the name of the main module.
readFlatCurryWithImportsInPath :: [String] -> String -> IO [Prog]
readFlatCurryWithImportsInPath loadpath modname =
  readFlatCurryFileWithImports False loadpath modname [".fcy"]

--- Reads a FlatCurry interface together with all its imported module
--- interfaces.
--- The argument is the name of the main module,
--- possibly with a directory prefix.
--- If there is no interface file but a FlatCurry file (suffix ".fcy"),
--- the FlatCurry file is read instead of the interface.
readFlatCurryIntWithImports :: String -> IO [Prog]
readFlatCurryIntWithImports modname = do
  loadpath <- getLoadPathForModule modname
  readFlatCurryFileWithImports False loadpath (baseName modname)
                               [".fint",".fcy"]

--- Reads a FlatCurry interface together with all its imported module interfaces
--- in a given load path.
--- The arguments are a load path and the name of the main module.
--- If there is no interface file but a FlatCurry file (suffix ".fcy"),
--- the FlatCurry file is read instead of the interface.
readFlatCurryIntWithImportsInPath :: [String] -> String -> IO [Prog]
readFlatCurryIntWithImportsInPath loadpath modname =
  readFlatCurryFileWithImports False loadpath modname [".fint",".fcy"]

-- Read a FlatCurry file together with its imported modules.
-- The arguments are the verbosity mode, the loadpath,
-- the name of the main module, and the possible suffixes
-- of the FlatCurry file (e.g., [".fint",".fcy"]).
readFlatCurryFileWithImports :: Bool -> [String] -> String -> [String]
                             -> IO [Prog]
readFlatCurryFileWithImports verb loadpath mod sfxs = do
  when verb $ putStr "Reading FlatCurry files "
  -- try to read the interface files directly
  eiMods <- tryReadFlatCurryFileWithImports verb loadpath mod sfxs
  either (\_ -> parseFlatCurryFileWithImports verb loadpath mod sfxs)
         return
         eiMods

-- Parse a FlatCurry file together with its imported modules.
-- The argument is the loadpath, the name of the main module, and the
-- possible suffixes of the FlatCurry file (e.g., [".fint",".fcy"]).
parseFlatCurryFileWithImports :: Bool -> [String] -> String -> [String]
                              -> IO [Prog]
parseFlatCurryFileWithImports verb loadpath modname suffixes = do
  when verb $
    putStrLn $ ">>>>> FlatCurry files not up-to-date, parsing module \""
                ++ modname ++ "\"..."
  callFrontendWithParams FCY
     (setQuiet True (setFullPath loadpath defaultParams)) modname
  when verb $ putStr "Reading FlatCurry files "
  eiMods <- tryReadFlatCurryFileWithImports verb loadpath modname suffixes
  return (either (error . notFound) id eiMods)
 where notFound mods = "FlatCurry file not found for the following module(s): "
                         ++ unwords mods

-- Read a FlatCurry file with all its imports. If all files could be read,
-- then `Right progs` is returned, otherwise `Left mods` where `mods` is
-- the list of modules that could *not* be read.
tryReadFlatCurryFileWithImports :: Bool -> [String] -> String -> [String]
                                -> IO (Either [String] [Prog])
tryReadFlatCurryFileWithImports verb loadpath modname suffixes =
  collect [modname] []
 where
  -- Collects all imported modules
  collect []         _       = when verb (putStrLn "done") >> return (Right [])
  collect (mod:mods) implist
    | mod `elem` implist     = collect mods implist
    | otherwise              = do
      mbProg <- tryReadFlatCurry verb loadpath mod suffixes
      case mbProg of
        Nothing                     -> return (Left [mod])
        Just prog@(Prog _ is _ _ _) -> do
          mbresults <- collect (mods ++ is) (mod:implist)
          return (either Left (Right . (prog :)) mbresults)

-- Read a single FlatCurry file for a module if it exists and is up-to-date
-- w.r.t. the source program. If no source exists, it is always assumed
-- to be up-to-date. If the source is newer then the FlatCurry file or
-- there is no FlatCurry file, the function returns `Nothing`.
tryReadFlatCurry :: Bool -> [String] -> String -> [String] -> IO (Maybe Prog)
tryReadFlatCurry verb loadpath modname suffixes = do
  mbSrc <- lookupModuleSourceInLoadPath modname
  case mbSrc of
    Nothing -> lookupFileInPath flatbasename suffixes loadpath >>=
               maybe (return Nothing) (liftIO Just  . readFlatCurryFile)
    Just (_,src) -> do
      mbFcy <- lookupFileInPath flatbasename suffixes loadpath
      case mbFcy of
        Nothing  -> return Nothing
        Just fcy -> do
          ctime <- getModificationTime src
          ftime <- getModificationTime fcy
          if ctime > ftime
            then return Nothing
            else do
              when verb $ putStr (normalise fcy ++ " ")
              Just `liftIO` readFlatCurryFile fcy
 where flatbasename = stripSuffix (flatCurryFileName modname)
