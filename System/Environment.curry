------------------------------------------------------------------------------
--- Library to access parts of the system environment.
---
--- @author Michael Hanus, Bernd Brassel, Bjoern Peemoeller
--- @version July 2012
--- @category general
------------------------------------------------------------------------------

module System.Environment
  ( getArgs, getEnv, getEnvironment, setEnv, unsetEnv, getProgName
  , getHostname, isPosix, isWindows
  ) where

import Data.Global

--- Returns the list of the program's command line arguments.
--- The program name is not included.

getArgs :: IO [String]
getArgs external

--- Returns the value of an environment variable.
--- The empty string is returned for undefined environment variables.

getEnv :: String -> IO String
getEnv evar = do
  envs <- getEnvironment
  maybe (prim_getEnviron $## evar) return (lookup evar envs)

prim_getEnviron :: String -> IO String
prim_getEnviron external

getEnvironment :: IO [(String, String)]
getEnvironment = readGlobal environ

--- internal state of environment variables set via setEnviron
environ :: Global [(String,String)]
environ = global [] Temporary

--- Set an environment variable to a value.
--- The new value will be passed to subsequent shell commands
--- (see <code>system</code>) and visible to subsequent calls to
--- <code>getEnv</code> (but it is not visible in the environment
--- of the process that started the program execution).

setEnv :: String -> String -> IO ()
setEnv evar val = do
  envs <- getEnvironment
  writeGlobal environ ((evar,val) : filter ((/=evar) . fst) envs)

--- Removes an environment variable that has been set by
--- <code>setEnv</code>.

unsetEnv :: String -> IO ()
unsetEnv evar = do
  envs <- getEnvironment
  writeGlobal environ (filter ((/=evar) . fst) envs)

--- Returns the hostname of the machine running this process.

getHostname :: IO String
getHostname external

--- Returns the name of the current program, i.e., the name of the
--- main module currently executed.

getProgName :: IO String
getProgName external

--- Is the underlying operating system a POSIX system (unix, MacOS)?
isPosix :: Bool
isPosix = not isWindows

--- Is the underlying operating system a Windows system?
isWindows :: Bool
isWindows external
