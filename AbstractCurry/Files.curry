-- ---------------------------------------------------------------------------
--- This library defines various I/O actions to read Curry programs and
--- transform them into the AbstractCurry representation and to write
--- AbstractCurry files.
---
--- Assumption: an abstract Curry program is stored in file with
--- extension `.acy` in the subdirectory `.curry`
---
--- @author Michael Hanus, Björn Peemöller
--- @version September 2015
--- @category meta
-- ---------------------------------------------------------------------------

module AbstractCurry.Files where

import AbstractCurry.Types
import Char         (isSpace)
import Directory    (doesFileExist)
import Distribution
import FilePath     ((<.>))
import Maybe        (isNothing)
import ReadShowTerm

-- ---------------------------------------------------------------------------

--- I/O action which parses a Curry program and returns the corresponding
--- typed Abstract Curry program.
--- Thus, the argument is the file name without suffix ".curry"
--- or ".lcurry") and the result is a Curry term representing this
--- program.
readCurry :: String -> IO CurryProg
readCurry prog = readCurryWithParseOptions prog (setQuiet True defaultParams)

--- I/O action which parses a Curry program and returns the corresponding
--- untyped Abstract Curry program.
--- Thus, the argument is the file name without suffix ".curry"
--- or ".lcurry") and the result is a Curry term representing this
--- program.
readUntypedCurry :: String -> IO CurryProg
readUntypedCurry prog =
  readUntypedCurryWithParseOptions prog (setQuiet True defaultParams)

--- I/O action which reads a typed Curry program from a file (with extension
--- ".acy") with respect to some parser options.
--- This I/O action is used by the standard action <CODE>readCurry</CODE>.
--- It is currently predefined only in Curry2Prolog.
--- @param progfile - the program file name (without suffix ".curry")
--- @param options - parameters passed to the front end

readCurryWithParseOptions :: String -> FrontendParams -> IO CurryProg
readCurryWithParseOptions progname options = do
  mbmoddir <- lookupModuleSourceInLoadPath progname
                >>= return . maybe Nothing (Just . fst)
  unless (isNothing mbmoddir) $
    callFrontendWithParams ACY options progname
  filename <- findFileInLoadPath (abstractCurryFileName progname)
  readAbstractCurryFile filename

--- I/O action which reads an untyped Curry program from a file (with extension
--- ".uacy") with respect to some parser options. For more details
--- see function 'readCurryWithParseOptions'
readUntypedCurryWithParseOptions :: String -> FrontendParams -> IO CurryProg
readUntypedCurryWithParseOptions progname options = do
  mbmoddir <- lookupModuleSourceInLoadPath progname
                >>= return . maybe Nothing (Just . fst)
  unless (isNothing mbmoddir) $
    callFrontendWithParams UACY options progname
  filename <- findFileInLoadPath (untypedAbstractCurryFileName progname)
  readAbstractCurryFile filename

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding AbstractCurry program.
abstractCurryFileName :: String -> String
abstractCurryFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "acy"

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding untyped AbstractCurry program.
untypedAbstractCurryFileName :: String -> String
untypedAbstractCurryFileName prog =
  inCurrySubdir (stripCurrySuffix prog) <.> "uacy"

--- I/O action which reads an AbstractCurry program from a file in ".acy"
--- format. In contrast to <CODE>readCurry</CODE>, this action does not parse
--- a source program. Thus, the argument must be the name of an existing
--- file (with suffix ".acy") containing an AbstractCurry program in ".acy"
--- format and the result is a Curry term representing this program.
--- It is currently predefined only in Curry2Prolog.
readAbstractCurryFile :: String -> IO CurryProg
readAbstractCurryFile filename = do
  exacy <- doesFileExist filename
  if exacy
   then readExistingACY filename
   else do let subdirfilename = inCurrySubdir filename
           exdiracy <- doesFileExist subdirfilename
           if exdiracy
            then readExistingACY subdirfilename
            else error ("EXISTENCE ERROR: AbstractCurry file '"++filename++
                        "' does not exist")
 where
   readExistingACY fname = do
     filecontents <- readFile fname
     let (line1,lines) = break (=='\n') filecontents
     if line1 == "{- "++version++" -}"
      then return (readUnqualifiedTerm ["AbstractCurry.Types","Prelude"] lines)
      else error $ "AbstractCurry: incompatible file found: "++fname

--- Tries to read an AbstractCurry file and returns
---
---  * Left err  , where err specifies the error occurred
---  * Right prog, where prog is the AbstractCurry program
tryReadACYFile :: String -> IO (Maybe CurryProg)
tryReadACYFile fn = do
  exists <- doesFileExist fn
  if exists
    then tryRead fn
    else do
      let fn' = inCurrySubdir fn
      exists' <- doesFileExist fn'
      if exists'
        then tryRead fn'
        else cancel
 where
  tryRead file = do
    src <- readFile file
    let (line1,lines) = break (=='\n') src
    if line1 /= "{- "++version++" -}"
      then error $ "AbstractCurry: incompatible file found: "++fn
      else case readsUnqualifiedTerm ["AbstractCurry.Types","Prelude"] lines of
        []       -> cancel
        [(p,tl)] -> if all isSpace tl
                      then return $ Just p
                      else cancel
        _        -> cancel
  cancel = return Nothing

--- Writes an AbstractCurry program into a file in ".acy" format.
--- The first argument must be the name of the target file
--- (with suffix ".acy").
writeAbstractCurryFile :: String -> CurryProg -> IO ()
writeAbstractCurryFile file prog = writeFile file (showTerm prog)

------------------------------------------------------------------------------
