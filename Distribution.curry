--------------------------------------------------------------------------------
--- This module contains functions to obtain information concerning the current
--- distribution of the Curry implementation, e.g.,
--- compiler version, load paths, front end.
---
--- @author Bernd Brassel, Michael Hanus, Bjoern Peemoeller, Finn Teegen
--- @version July 2017
--- @category general
--------------------------------------------------------------------------------

module Distribution (
  curryCompiler, curryCompilerMajorVersion, curryCompilerMinorVersion,
  curryRuntime, curryRuntimeMajorVersion, curryRuntimeMinorVersion,
  baseVersion, installDir, stripCurrySuffix, modNameToPath,
  currySubdir, inCurrySubdir, addCurrySubdir,
  rcFileName, rcFileContents, getRcVar, getRcVars,

  joinModuleIdentifiers, splitModuleIdentifiers, splitModuleFileName,
  inCurrySubdirModule,

  sysLibPath, getLoadPathForModule,
  lookupModuleSourceInLoadPath, lookupModuleSource,

  FrontendTarget(..),

  FrontendParams, defaultParams, rcParams,
  quiet, extended, cpp, definitions, overlapWarn, fullPath, htmldir, logfile,
  specials, setQuiet, setExtended, setCpp, setDefinitions, setOverlapWarn,
  setFullPath, setHtmlDir, setLogfile, addTarget, setSpecials,

  callFrontend, callFrontendWithParams
  ) where

import List         (intercalate, nub, split)
import Char         (toLower, toUpper)
import Directory    (doesFileExist, getHomeDirectory)
import FileGoodies  (lookupFileInPath, getFileInPath, fileSuffix, stripSuffix)
import FilePath     ( FilePath, (</>), (<.>), addTrailingPathSeparator
                    , dropFileName, joinPath, normalise, splitDirectories
                    , splitExtension, splitFileName, splitSearchPath
                    , takeFileName
                    )
import IO
import PropertyFile
import System

-----------------------------------------------------------------
-- Compiler and run-time environment name and version
-----------------------------------------------------------------

-- if you do not use other functions but
-- if-then-else, and the _Prelude_ functions
-- (<), (>), (<=), (>=), (==)
-- directly on the following constants,
-- the compiler might be able to eliminate
-- them at compile time.

--- The name of the Curry compiler (e.g., "pakcs" or "kics2").
curryCompiler :: String
curryCompiler external

--- The major version number of the Curry compiler.
curryCompilerMajorVersion :: Int
curryCompilerMajorVersion external

--- The minor version number of the Curry compiler.
curryCompilerMinorVersion :: Int
curryCompilerMinorVersion external

--- The name of the run-time environment (e.g., "sicstus", "swi", or "ghc")
curryRuntime :: String
curryRuntime external

--- The major version number of the Curry run-time environment.
curryRuntimeMajorVersion :: Int
curryRuntimeMajorVersion external

--- The minor version number of the Curry run-time environment.
curryRuntimeMinorVersion :: Int
curryRuntimeMinorVersion external

--- The version number of the base libraries (e.g., "1.0.5").
baseVersion :: String
baseVersion external

--- Path of the main installation directory of the Curry compiler.
installDir :: FilePath
installDir external

---------------------------------------------------
-- retrieving user specified options from rc file
---------------------------------------------------

--- The name of the file specifying configuration parameters of the
--- current distribution. This file must have the usual format of
--- property files (see description in module PropertyFile).
rcFileName :: IO String
rcFileName = getHomeDirectory >>= return . (</> rcFile)
  where rcFile = '.' : curryCompiler ++ "rc"

--- Returns the current configuration parameters of the distribution.
--- This action yields the list of pairs (var,val).
rcFileContents :: IO [(String,String)]
rcFileContents = rcFileName >>= readPropertyFile

--- Look up a specific configuration variable as specified by user in his rc file.
--- Uppercase/lowercase is ignored for the variable names.
getRcVar :: String -> IO (Maybe String)
getRcVar var = getRcVars [var] >>= return . head

--- Look up configuration variables as specified by user in his rc file.
--- Uppercase/lowercase is ignored for the variable names.
getRcVars :: [String] -> IO [Maybe String]
getRcVars vars = do
  rcs <- rcFileContents
  return (map (flip lookup (map (\ (a, b) -> (map toLower a, b)) rcs))
              (map (map toLower) vars))

-----------------------------------------------------------
--- Functions for handling file names of Curry modules
-----------------------------------------------------------

type ModuleIdent = String

--- Split the `FilePath` of a module into the directory prefix and the
--- `FilePath` corresponding to the module name.
--- For instance, the call `splitModuleFileName "Data.Set" "lib/Data/Set.curry"`
--- evaluates to `("lib", "Data/Set.curry")`.
--- This can be useful to compute output directories while retaining the
--- hierarchical module structure.
splitModuleFileName :: ModuleIdent -> FilePath -> (FilePath, FilePath)
splitModuleFileName mid fn = case splitModuleIdentifiers mid of
  [_] -> splitFileName fn
  ms  -> let (base, ext) = splitExtension fn
             dirs        = splitDirectories base
             (pre , suf) = splitAt (length dirs - length ms) dirs
             path        = if null pre then ""
                                       else addTrailingPathSeparator (joinPath pre)
         in  (path, joinPath suf <.> ext)

--- Split up the components of a module identifier. For instance,
--- `splitModuleIdentifiers "Data.Set"` evaluates to `["Data", "Set"]`.
splitModuleIdentifiers :: ModuleIdent -> [String]
splitModuleIdentifiers = split (=='.')

--- Join the components of a module identifier. For instance,
--- `joinModuleIdentifiers ["Data", "Set"]` evaluates to `"Data.Set"`.
joinModuleIdentifiers :: [String] -> ModuleIdent
joinModuleIdentifiers = foldr1 combine
  where combine xs ys = xs ++ '.' : ys

--- Strips the suffix ".curry" or ".lcurry" from a file name.
stripCurrySuffix :: String -> String
stripCurrySuffix s =
  if fileSuffix s `elem` ["curry","lcurry"]
  then stripSuffix s
  else s

--- A module path consists of a directory prefix (which can be omitted)
--- and a module name (which can be hierarchical). For instance, the
--- following strings are module paths in Unix-based systems:
---
---     HTML
---     Data.Number.Int
---     curry/Data.Number.Int
type ModulePath = String

--- Transforms a hierarchical module name into a path name, i.e.,
--- replace the dots in the name by directory separator chars.
modNameToPath :: ModuleIdent -> String
modNameToPath = foldr1 (</>) . split (=='.')

--- Name of the sub directory where auxiliary files (.fint, .fcy, etc)
--- are stored.
currySubdir :: FilePath
currySubdir = ".curry"

--- Transforms a path to a module name into a file name
--- by adding the `currySubDir` to the path and transforming
--- a hierarchical module name into a path.
--- For instance, `inCurrySubdir "mylib/Data.Char"` evaluates to
--- `"mylib/.curry/Data/Char"`.
inCurrySubdir :: FilePath -> FilePath
inCurrySubdir filename =
  let (base,file) = splitFileName filename
   in base </> currySubdir </> modNameToPath file

--- Transforms a file name by adding the currySubDir to the file name.
--- This version respects hierarchical module names.
inCurrySubdirModule :: ModuleIdent -> FilePath -> FilePath
inCurrySubdirModule m fn = let (dirP, modP) = splitModuleFileName m fn
                           in  dirP </> currySubdir </> modP

--- Transforms a directory name into the name of the corresponding
--- sub directory containing auxiliary files.
addCurrySubdir :: FilePath -> FilePath
addCurrySubdir dir = dir </> currySubdir

-----------------------------------------------------------
--- finding files in correspondence to compiler load path
-----------------------------------------------------------

--- Returns the current path (list of directory names) of the
--- system libraries.
sysLibPath :: [String]
sysLibPath = case curryCompiler of
  "pakcs" -> [installDir </> "lib"]
  "kics"  -> [installDir </> "src" </> "lib"]
  "kics2" -> [installDir </> "lib"]
  _       -> error "Distribution.sysLibPath: unknown curryCompiler"

--- Returns the current path (list of directory names) that is
--- used for loading modules w.r.t. a given module path.
--- The directory prefix of the module path (or "." if there is
--- no such prefix) is the first element of the load path and the
--- remaining elements are determined by the environment variable
--- CURRYRPATH and the entry "libraries" of the system's rc file.
getLoadPathForModule :: ModulePath -> IO [String]
getLoadPathForModule modpath = do
  mblib  <- getRcVar "libraries"
  let fileDir = dropFileName modpath
  if curryCompiler `elem` ["pakcs","kics","kics2"] then
    do currypath <- getEnviron "CURRYPATH"
       let llib = maybe [] (\l -> if null l then [] else splitSearchPath l)
                        mblib
       return $ (fileDir : (if null currypath
                            then []
                            else splitSearchPath currypath) ++
                           llib ++ sysLibPath)
    else error "Distribution.getLoadPathForModule: unknown curryCompiler"

--- Returns a directory name and the actual source file name for a module
--- by looking up the module source in the current load path.
--- If the module is hierarchical, the directory is the top directory
--- of the hierarchy.
--- Returns Nothing if there is no corresponding source file.
lookupModuleSourceInLoadPath :: ModulePath -> IO (Maybe (String,String))
lookupModuleSourceInLoadPath modpath = do
  loadpath <- getLoadPathForModule modpath
  lookupModuleSource loadpath modpath

--- Returns a directory name and the actual source file name for a module
--- by looking up the module source in the load path provided as the
--- first argument.
--- If the module is hierarchical, the directory is the top directory
--- of the hierarchy.
--- Returns Nothing if there is no corresponding source file.
lookupModuleSource :: [String] -> String -> IO (Maybe (String,String))
lookupModuleSource loadpath mod = lookupSourceInPath loadpath
 where
  fn       = takeFileName mod
  fnlcurry = modNameToPath fn ++ ".lcurry"
  fncurry  = modNameToPath fn ++ ".curry"

  lookupSourceInPath [] = return Nothing
  lookupSourceInPath (dir:dirs) = do
    lcurryExists <- doesFileExist (dir </> fnlcurry)
    if lcurryExists then return (Just (dir, dir </> fnlcurry)) else do
     curryExists <- doesFileExist (dir </> fncurry)
     if curryExists then return (Just (dir, dir </> fncurry))
                    else lookupSourceInPath dirs

-------------------------------------------------------------------
-- calling the front end
-------------------------------------------------------------------

--- Data type for representing the different target files that can be produced
--- by the front end of the Curry compiler.
--- @cons FCY  - FlatCurry file ending with .fcy
--- @cons TFCY - Typed FlatCurry file ending with .tfcy
--- @cons FINT - FlatCurry interface file ending with .fint
--- @cons ACY  - AbstractCurry file ending with .acy
--- @cons UACY - Untyped (without type checking) AbstractCurry file ending with .uacy
--- @cons HTML - colored HTML representation of source program
--- @cons CY   - source representation employed by the frontend
--- @cons TOKS - token stream of source program
data FrontendTarget = FCY | TFCY | FINT | ACY | UACY | HTML | CY | TOKS
  deriving Eq

--- Abstract data type for representing parameters supported by the front end
--- of the Curry compiler.
-- The parameters are of the form
-- FrontendParams Quiet Extended Cpp NoOverlapWarn FullPath HtmlDir LogFile Specials
-- where
--   Quiet         - work silently
--   Extended      - support extended Curry syntax
--   Cpp           - enable conditional compiling
--   Definitions   - definitions for conditional compiling
--   OverlapWarn   - warn for overlapping rules
--   FullPath dirs - the complete list of directory names for loading modules
--   HtmlDir file  - output directory (only relevant for HTML target)
--   LogFile file  - store all output (including errors) of the front end in file
--   Targets       - additional targets for the front end
--   Specials      - additional special parameters (use with care!)
data FrontendParams =
  FrontendParams Bool
                 Bool
                 Bool
                 [(String, Int)]
                 Bool
                 (Maybe [String])
                 (Maybe String)
                 (Maybe String)
                 [FrontendTarget]
                 String

--- The default parameters of the front end.
defaultParams :: FrontendParams
defaultParams =
  FrontendParams False True False [] True Nothing Nothing Nothing [] ""

--- The default parameters of the front end as configured by the compiler
--- specific resource configuration file.
rcParams :: IO FrontendParams
rcParams = do
  [mbExtended,mbOverlapWarn] <- getRcVars ["curryextensions","warnoverlapping"]
  return $ setExtended    (mbExtended    /= Just "no")
         $ setOverlapWarn (mbOverlapWarn /= Just "no")
         $ defaultParams

--- Set quiet mode of the front end.
setQuiet :: Bool -> FrontendParams -> FrontendParams
setQuiet s (FrontendParams _ t u v w x y z ts sp) =
  FrontendParams s t u v w x y z ts sp

--- Set extended mode of the front end.
setExtended :: Bool -> FrontendParams -> FrontendParams
setExtended s (FrontendParams a _ u v w x y z ts sp) =
  FrontendParams a s u v w x y z ts sp

--- Set cpp mode of the front end.
setCpp :: Bool -> FrontendParams -> FrontendParams
setCpp s (FrontendParams a b _ v w x y z ts sp) =
  FrontendParams a b s v w x y z ts sp

--- Set cpp definitions of the front end.
setDefinitions :: [(String, Int)] -> FrontendParams -> FrontendParams
setDefinitions s (FrontendParams a b c _ w x y z ts sp) =
  FrontendParams a b c s w x y z ts sp

--- Set overlap warn mode of the front end.
setOverlapWarn :: Bool -> FrontendParams -> FrontendParams
setOverlapWarn s (FrontendParams a b c d _ x y z ts sp) =
  FrontendParams a b c d s x y z ts sp

--- Set the full path of the front end.
--- If this parameter is set, the front end searches all modules
--- in this path (instead of using the default path).
setFullPath :: [String] -> FrontendParams -> FrontendParams
setFullPath s (FrontendParams a b c d e _ y z ts sp) =
  FrontendParams a b c d e (Just s) y z ts sp

--- Set the htmldir parameter of the front end.
--- Relevant for HTML generation.
setHtmlDir :: String -> FrontendParams -> FrontendParams
setHtmlDir s (FrontendParams a b c d e f _ z ts sp) =
  FrontendParams a b c d e f (Just s) z ts sp

--- Set the logfile parameter of the front end.
--- If this parameter is set, all messages produced by the front end
--- are stored in this file.
setLogfile :: String -> FrontendParams -> FrontendParams
setLogfile s (FrontendParams a b c d e f g _ ts sp) =
  FrontendParams a b c d e f g (Just s) ts sp

--- Set additional specials parameters of the front end.
--- These parameters are specific for the current front end and
--- should be used with care, since their form might change in the future.
setSpecials :: String -> FrontendParams -> FrontendParams
setSpecials s (FrontendParams a b c d e f g h ts _) =
  FrontendParams a b c d e f g h ts s

--- Add an additional front end target.
addTarget :: FrontendTarget -> FrontendParams -> FrontendParams
addTarget t (FrontendParams a b c d e f g h ts sp) =
  FrontendParams a b c d e f g h (t:ts) sp

--- Returns the value of the "quiet" parameter.
quiet :: FrontendParams -> Bool
quiet (FrontendParams x _ _ _ _ _ _ _ _ _) = x

--- Returns the value of the "extended" parameter.
extended :: FrontendParams -> Bool
extended (FrontendParams _ x _ _ _ _ _ _ _ _) = x

--- Returns the value of the "cpp" parameter.
cpp :: FrontendParams -> Bool
cpp (FrontendParams _ _ x _ _ _ _ _ _ _) = x

--- Returns the value of the "cpp" parameter.
definitions :: FrontendParams -> [(String, Int)]
definitions (FrontendParams _ _ _ x _ _ _ _ _ _) = x

--- Returns the value of the "overlapWarn" parameter.
overlapWarn :: FrontendParams -> Bool
overlapWarn (FrontendParams _ _ _ _ x _ _ _ _ _) = x

--- Returns the full path parameter of the front end.
fullPath :: FrontendParams -> Maybe [String]
fullPath (FrontendParams _ _ _ _ _ x _ _ _ _) = x

--- Returns the htmldir parameter of the front end.
htmldir :: FrontendParams -> Maybe String
htmldir  (FrontendParams _ _ _ _ _ _ x _ _ _) = x

--- Returns the logfile parameter of the front end.
logfile :: FrontendParams -> Maybe String
logfile  (FrontendParams _ _ _ _ _ _ _ x _ _) = x

--- Returns the special parameters of the front end.
targets :: FrontendParams -> [FrontendTarget]
targets (FrontendParams _ _ _ _ _ _ _ _ x _) = x

--- Returns the special parameters of the front end.
specials :: FrontendParams -> String
specials (FrontendParams _ _ _ _ _ _ _ _ _ x) = x

--- In order to make sure that compiler generated files (like .fcy, .fint, .acy)
--- are up to date, one can call the front end of the Curry compiler
--- with this action.
--- If the front end returns with an error, an exception is raised.
--- @param target - the kind of target file to be generated
--- @param progname - the name of the main module of the application to be compiled
callFrontend :: FrontendTarget -> String -> IO ()
callFrontend target p = do
  params <- rcParams
  callFrontendWithParams target params p

--- In order to make sure that compiler generated files (like .fcy, .fint, .acy)
--- are up to date, one can call the front end of the Curry compiler
--- with this action where various parameters can be set.
--- If the front end returns with an error, an exception is raised.
--- @param target - the kind of target file to be generated
--- @param params - parameters for the front end
--- @param modpath - the name of the main module possibly prefixed with a
---                  directory where this module resides
callFrontendWithParams :: FrontendTarget -> FrontendParams -> ModulePath
                       -> IO ()
callFrontendWithParams target params modpath = do
  parsecurry <- callParseCurry
  let lf      = maybe "" id (logfile params)
      tgts    = nub (target : targets params)
      syscall = unwords $ [parsecurry] ++ map showFrontendTarget tgts ++
                          [showFrontendParams, cppParams, takeFileName modpath]
  retcode <- if null lf
             then system syscall
             else system (syscall ++ " > " ++ lf ++ " 2>&1")
  if retcode == 0
   then done
   else ioError (userError "Illegal source program")
 where
   callParseCurry = do
     path <- maybe (getLoadPathForModule modpath) return (fullPath params)
     return (quote (installDir </> "bin" </> curryCompiler ++ "-frontend")
             ++ concatMap ((" -i" ++) . quote) path)

   quote s = '"' : s ++ "\""

   showFrontendTarget FCY  = "--flat"
   showFrontendTarget TFCY = "--typed-flat"
   showFrontendTarget FINT = "--flat"
   showFrontendTarget ACY  = "--acy"
   showFrontendTarget UACY = "--uacy"
   showFrontendTarget HTML = "--html"
   showFrontendTarget CY   = "--parse-only"
   showFrontendTarget TOKS = "--tokens"

   showFrontendParams = unwords
    [ if quiet       params then runQuiet     else ""
    , if extended    params then "--extended" else ""
    , if cpp         params then "--cpp"      else ""
    , if overlapWarn params then ""           else "--no-overlap-warn"
    , maybe "" ("--htmldir="++) (htmldir params)
    , specials params
    ]

   runQuiet = "--no-verb --no-warn --no-overlap-warn"

   cppParams = intercalate " " $ map showDefinition (definitions params)

   showDefinition (s, v) = "-D__" ++ s ++ "__=" ++ show v
