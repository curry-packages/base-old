import qualified Installation  as I
import qualified Curry_Prelude as CP

external_d_C_curryCompiler ::  ConstStore -> CP.C_String
external_d_C_curryCompiler _ = toCurry "kics2"

external_d_C_curryCompilerMajorVersion ::  ConstStore -> CP.C_Int
external_d_C_curryCompilerMajorVersion _ = toCurry I.majorVersion

external_d_C_curryCompilerMinorVersion ::  ConstStore -> CP.C_Int
external_d_C_curryCompilerMinorVersion _ = toCurry I.minorVersion

external_d_C_curryRuntime ::  ConstStore -> CP.C_String
external_d_C_curryRuntime _ = toCurry I.runtime

external_d_C_curryRuntimeMajorVersion ::  ConstStore -> CP.C_Int
external_d_C_curryRuntimeMajorVersion _ = toCurry I.runtimeMajor

external_d_C_curryRuntimeMinorVersion ::  ConstStore -> CP.C_Int
external_d_C_curryRuntimeMinorVersion _ = toCurry I.runtimeMinor

external_d_C_installDir ::  ConstStore -> CP.C_String
external_d_C_installDir _ = toCurry I.installDir
