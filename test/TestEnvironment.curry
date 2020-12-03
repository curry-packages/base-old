------------------------------------------------------------------------------
--- Some tests for library System.Environment
---
--- To run all tests automatically by CurryCheck, use the command:
---
---     > curry-check TestEnvironment
------------------------------------------------------------------------------

import System.Environment
import Test.Prop

-- Testing environment variable handling:

evar = "asd123"

testGetUndefinedEnviron = (getEnv evar) `returns` ""

testSetEnviron = (setEnv evar "SET" >> getEnv evar) `returns` "SET"

testUnsetEnviron = (unsetEnv evar >> getEnv evar) `returns` ""
