import System.Directory
import System.IO
import System.Time

import qualified Curry_Prelude as CP

external_d_C_prim_doesFileExist :: CP.C_String -> Cover -> ConstStore 
                                -> CP.C_IO CP.C_Bool
external_d_C_prim_doesFileExist s _ _ = toCurry doesFileExist s

external_d_C_prim_doesDirectoryExist :: CP.C_String -> Cover -> ConstStore 
                                     -> CP.C_IO CP.C_Bool
external_d_C_prim_doesDirectoryExist s _ _ = toCurry doesDirectoryExist s

external_d_C_prim_fileSize :: CP.C_String -> Cover -> ConstStore 
                           -> CP.C_IO CP.C_Int
external_d_C_prim_fileSize s _ _ = toCurry
  (\f -> do h <- openFile f ReadMode
            i <- hFileSize h
            hClose h
            return i
  ) s

external_d_C_prim_getModificationTime :: CP.C_String -> Cover -> ConstStore
                                      -> CP.C_IO Curry_Time.C_ClockTime
external_d_C_prim_getModificationTime s _ _ = toCurry getModificationTime s

external_d_C_getCurrentDirectory :: Cover -> ConstStore -> CP.C_IO (CP.C_String)
external_d_C_getCurrentDirectory _ _ = toCurry getCurrentDirectory

external_d_C_prim_setCurrentDirectory :: CP.C_String -> Cover -> ConstStore
                                      -> CP.C_IO CP.OP_Unit
external_d_C_prim_setCurrentDirectory s _ _ = toCurry setCurrentDirectory s

external_d_C_prim_getDirectoryContents :: CP.C_String -> Cover -> ConstStore
                                       -> CP.C_IO (CP.OP_List (CP.C_String))
external_d_C_prim_getDirectoryContents s _ _ = toCurry getDirectoryContents s

external_d_C_prim_createDirectory :: CP.C_String -> Cover -> ConstStore
                                  -> CP.C_IO CP.OP_Unit
external_d_C_prim_createDirectory s _ _ = toCurry createDirectory s

external_d_C_prim_removeFile :: CP.C_String -> Cover -> ConstStore 
                             -> CP.C_IO CP.OP_Unit
external_d_C_prim_removeFile s _ _ = toCurry removeFile s

external_d_C_prim_removeDirectory :: CP.C_String -> Cover -> ConstStore 
                                  -> CP.C_IO CP.OP_Unit
external_d_C_prim_removeDirectory s _ _ = toCurry removeDirectory s

external_d_C_prim_renameFile :: CP.C_String -> CP.C_String -> Cover -> ConstStore 
                             -> CP.C_IO CP.OP_Unit
external_d_C_prim_renameFile s1 s2 _ _ = toCurry renameFile s1 s2

external_d_C_prim_renameDirectory :: CP.C_String -> CP.C_String 
                                  -> Cover -> ConstStore 
                                  -> CP.C_IO CP.OP_Unit
external_d_C_prim_renameDirectory s1 s2 _ _= toCurry renameDirectory s1 s2
