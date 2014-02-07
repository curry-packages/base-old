--------------------------------------------------------------------------------
--- This module contains functions to obtain information concerning the current
--- distribution of the Curry implementation.
--- Most of the information is based on the external constants 
--- <b>curryCompiler...</b>.
---
--- @author Bernd Brassel, Michael Hanus, Bjoern Peemoeller
--- @version February 2014
--------------------------------------------------------------------------------

module Distribution (
  curryCompiler,curryCompilerMajorVersion,curryCompilerMinorVersion,
  curryRuntime,curryRuntimeMajorVersion,curryRuntimeMinorVersion,
  installDir,currySubdir,inCurrySubdir,addCurrySubdir,
  
  rcFileName,rcFileContents,getRcVar,getRcVars,

  findFileInLoadPath,lookupFileInLoadPath,
  readFirstFileInLoadPath,getLoadPath,getLoadPathForFile,

  FrontendTarget(..), 
  
  FrontendParams, defaultParams, rcParams,
  quiet, extended, overlapWarn, fullPath, outfile, logfile, specials,
  setQuiet, setExtended, setOverlapWarn, setFullPath, setOutfile, setLogfile,
  setSpecials,

  callFrontend,callFrontendWithParams
  ) where

import List(intersperse)
import Char(toLower)
import System
import IO
import FileGoodies
import FilePath ((</>))
import PropertyFile

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

---------------------------------------------------
-- retrieving user specified options from rc file
---------------------------------------------------

--- The name of the file specifying configuration parameters of the
--- current distribution. This file must have the usual format of
--- property files (see description in module PropertyFile).
rcFileName :: IO String
rcFileName = getEnviron "HOME" >>= return . (</> rcFile)
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
  return (map (flip lookup (map (\ (a,b)->(map toLower a,b)) rcs))
              (map (map toLower) vars))

-----------------------------------------------------------
--- finding files in correspondence to compiler load path
-----------------------------------------------------------

--- Name of the main installation directory of the Curry compiler.
installDir :: String
installDir external

--- Name of the sub directory where auxiliary files (.fint, .fcy, etc)
--- are stored.
currySubdir :: String
currySubdir = ".curry"

--- Transforms a file name by adding the currySubDir to the file name.
inCurrySubdir :: String -> String
inCurrySubdir filename =
  let (base,file) = splitDirectoryBaseName filename
   in base </> currySubdir </> file

--- Transforms a directory name into the name of the corresponding
--- sub directory containing auxiliary files.
addCurrySubdir :: String -> String
addCurrySubdir dir = dir </> currySubdir

--- Returns the current path (list of directory names) of the
--- system libraries.
getSysLibPath :: IO [String]
getSysLibPath = case curryCompiler of
  "pakcs" -> return [installDir </> "lib", installDir </> "lib" </> "meta"]
  "kics"  -> return [installDir </> "src" </> "lib"]
  "kics2" -> return [installDir </> "lib", installDir </> "lib" </> "meta"]
  _ -> error "Distribution.getSysLibPath: unknown curryCompiler"


--- Adds a directory name to a file by looking up the current load path.
--- An error message is delivered if there is no such file.
lookupFileInLoadPath :: String -> IO (Maybe String)
lookupFileInLoadPath fn =
  getLoadPathForFile fn >>= lookupFileInPath (baseName fn) [""]

--- Adds a directory name to a file by looking up the current load path.
--- An error message is delivered if there is no such file.
findFileInLoadPath :: String -> IO String
findFileInLoadPath fn =
  getLoadPathForFile fn >>= getFileInPath (baseName fn) [""]

--- Returns the contents of the file found first in the current load path.
--- An error message is delivered if there is no such file.
readFirstFileInLoadPath :: String -> IO String
readFirstFileInLoadPath fn = findFileInLoadPath fn >>= readFile 

--- Returns the current path (list of directory names) that is
--- used for loading modules.
getLoadPath :: IO [String]
getLoadPath = getLoadPathForFile "xxx"

--- Returns the current path (list of directory names) that is
--- used for loading modules w.r.t. a given main module file.
--- The directory prefix of the module file (or "." if there is
--- no such prefix) is the first element of the load path and the
--- remaining elements are determined by the environment variable
--- CURRYRPATH and the entry "libraries" of the system's rc file.
getLoadPathForFile :: String -> IO [String]
getLoadPathForFile file = do
  syslib <- getSysLibPath
  mblib  <- getRcVar "libraries"
  let fileDir = dirName file
  if curryCompiler `elem` ["pakcs","kics","kics2"] then
    do currypath <- getEnviron "CURRYPATH"
       let llib = maybe [] splitPath mblib
       return (addCurrySubdirs (fileDir :
                                   (if null currypath
                                    then []
                                    else splitPath currypath) ++
                                    llib ++ syslib))

    else error "Distribution.getLoadPathForFile: unknown curryCompiler"
 where
  addCurrySubdirs = concatMap (\ d -> [d, addCurrySubdir d])

-------------------------------------------------------------------
-- calling the front end
-------------------------------------------------------------------

--- Data type for representing the different target files that can be produced
--- by the front end of the Curry compiler.
--- @cons FCY  - FlatCurry file ending with .fcy
--- @cons FINT - FlatCurry interface file ending with .fint
--- @cons ACY  - AbstractCurry file ending with .acy
--- @cons UACY - Untyped (without type checking) AbstractCurry file ending with .uacy
--- @cons HTML - colored HTML representation of source program
--- @cons CY   - source representation employed by the frontend
data FrontendTarget = FCY | FINT | ACY | UACY | HTML | CY

--- Abstract data type for representing parameters supported by the front end
--- of the Curry compiler.
-- The parameters are of the form
-- FrontendParams Quiet Extended NoOverlapWarn FullPath OutFile LogFile Specials
-- where
--   Quiet         - work silently
--   Extended      - support extended Curry syntax
--   OverlapWarn   - warn for overlapping rules
--   FullPath dirs - the complete list of directory names for loading modules
--   OutFile file  - output file (currently, only relevant for HTML target)
--   LogFile file  - store all output (including errors) of the front end in file
--   Specials      - additional special parameters (use with care!)
data FrontendParams =
  FrontendParams Bool
                 Bool
                 Bool
                 (Maybe [String])
                 (Maybe String)
                 (Maybe String)
                 String

--- The default parameters of the front end.
defaultParams :: FrontendParams
defaultParams = FrontendParams False True True Nothing Nothing Nothing ""

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
setQuiet s (FrontendParams _ v w x y z sp) = FrontendParams s v w x y z sp

--- Set extended mode of the front end.
setExtended :: Bool -> FrontendParams -> FrontendParams
setExtended s (FrontendParams a _ w x y z sp) = FrontendParams a s w x y z sp

--- Set overlap warn mode of the front end.
setOverlapWarn :: Bool -> FrontendParams -> FrontendParams
setOverlapWarn s (FrontendParams a b _ x y z sp) = FrontendParams a b s x y z sp

--- Set the full path of the front end.
--- If this parameter is set, the front end searches all modules
--- in this path (instead of using the default path).
setFullPath ::  [String] -> FrontendParams -> FrontendParams 
setFullPath s (FrontendParams a b c _ y z sp) =
  FrontendParams a b c (Just s) y z sp

--- Set the outfile parameter of the front end.
--- Relevant for HTML generation.
setOutfile ::  String -> FrontendParams -> FrontendParams 
setOutfile  s (FrontendParams a b c d _ z sp) =
  FrontendParams a b c d (Just s) z sp

--- Set the logfile parameter of the front end.
--- If this parameter is set, all messages produced by the front end
--- are stored in this file.
setLogfile ::  String -> FrontendParams -> FrontendParams 
setLogfile  s (FrontendParams a b c d e _ sp) =
  FrontendParams a b c d e (Just s) sp

--- Set additional specials parameters of the front end.
--- These parameters are specific for the current front end and
--- should be used with care, since their form might change in the future.
setSpecials :: String -> FrontendParams -> FrontendParams 
setSpecials sp (FrontendParams a b c d e z _) =
  FrontendParams a b c d e z sp

--- Returns the value of the "quiet" parameter.
quiet :: FrontendParams -> Bool
quiet (FrontendParams x _ _ _ _ _ _) = x

--- Returns the value of the "extended" parameter.
extended :: FrontendParams -> Bool
extended (FrontendParams _ x _ _ _ _ _) = x

--- Returns the value of the "overlapWarn" parameter.
overlapWarn :: FrontendParams -> Bool
overlapWarn (FrontendParams _ _ x _ _ _ _) = x

--- Returns the full path parameter of the front end.
fullPath :: FrontendParams -> Maybe [String]
fullPath (FrontendParams _ _ _ x _ _ _) = x

--- Returns the outfile parameter of the front end.
outfile :: FrontendParams -> Maybe String
outfile  (FrontendParams _ _ _ _ x _ _) = x

--- Returns the logfile parameter of the front end.
logfile :: FrontendParams -> Maybe String
logfile  (FrontendParams _ _ _ _ _ x _) = x

--- Returns the special parameters of the front end.
specials :: FrontendParams -> String
specials (FrontendParams _ _ _ _ _ _ x) = x

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
--- @param progname - the name of the main module of the application to be compiled
callFrontendWithParams :: FrontendTarget -> FrontendParams -> String -> IO ()
callFrontendWithParams target params progname = do
  parsecurry <- callParseCurry
  let lf      = maybe "" id (logfile params)
      syscall = parsecurry ++ " " ++ showFrontendTarget target
                           ++ " " ++ showFrontendParams 
                           ++ " " ++ progname
  retcode <- if null lf
             then system syscall
             else system (syscall ++ " > " ++ lf ++ " 2>&1")
  if retcode == 0
   then done
   else ioError (userError "Illegal source program")
 where
   callParseCurry = case curryCompiler of
     "pakcs" -> return ("\"" ++ installDir </> "bin" </> "parsecurry\"")
     "kics"  -> do path <- maybe getLoadPath return (fullPath params)
                   return ("\"" ++ installDir </> "bin" </> "parsecurry\""++
                           concatMap (" -i"++) path)
     "kics2"  -> do path <- maybe getLoadPath return (fullPath params)
                    return ("\"" ++ installDir </> "bin" </> "cymake\"" ++
                            concatMap (\d->" -i\""++d++"\"") path)
     _ -> error "Distribution.callFrontend: unknown curryCompiler"

   showFrontendTarget FCY  = "--flat"
   showFrontendTarget FINT = "--flat"
   showFrontendTarget ACY  = "--acy"
   showFrontendTarget UACY = "--uacy"
   showFrontendTarget HTML = "--html"
   showFrontendTarget CY   = "--parse-only"

   showFrontendParams = unwords
    [ if quiet       params then runQuiet     else ""
    , if extended    params then "--extended" else ""
    , if overlapWarn params then ""           else "--no-overlap-warn"
    , maybe "" ("-o "++) (outfile params)
    , maybe "" (\p -> if curryCompiler=="pakcs"
                      then "--fullpath " ++ concat (intersperse ":" p)
                      else "")
              (fullPath params)
    , specials params
    ]

   runQuiet = if curryCompiler=="pakcs"
              then "--quiet"
              else "--no-verb --no-warn --no-overlap-warn" -- kics(2)

rcErr :: String -> a -> IO a
rcErr s x = hPutStrLn stderr (s ++ " undefined in rc file") >> return x
