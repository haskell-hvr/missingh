-- © 2001 Peter Thiemann
module MissingH.Wash.Utility.Unique (inventStdKey, inventKey, inventFilePath) where

import Random
import IO
import Directory
import MissingH.Wash.Utility.Auxiliary
import List
import Monad
import MissingH.Wash.Utility.Locking

registryDir = "/tmp/Unique/"

-- |Creates a random string of 20 letters and digits.
inventStdKey :: IO String
inventStdKey = inventKey 20 stdKeyChars

stdKeyChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

-- |Creates a unique string from a given length and alphabet.
inventKey :: Int -> String -> IO String
inventKey len chars =
  do g <- newStdGen
     let candidate = take len $ map (chars !!) $ randomRs (0, length chars - 1) g
	 dirname = registryDir ++ candidate
     catch (do createDirectory dirname
	       return candidate)
	   (\ ioe -> 
	   if isAlreadyExistsError ioe then
	      -- might want to check here for timeout
	      inventKey len chars
	   else if isDoesNotExistError ioe then
	     do assertDirectoryExists registryDir (return ())
		setPermissions registryDir (Permissions True True True True)
		inventKey len chars
	   else do hPutStrLn stderr ("inventKey could not create " ++ show dirname)
		   ioError ioe)

-- |Create a unique temporary file name
inventFilePath :: IO String
inventFilePath =
  do key <- inventStdKey
     return (registryDir ++ key ++ "/f")

-- obsolete. registryFile is a bottleneck.

registryFile = registryDir ++ "REGISTRY"

inventKey' :: Int -> String -> IO String
inventKey' len chars =
  do g <- newStdGen
     let candidate = take len $ map (chars !!) $ randomRs (0, length chars - 1) g
     obtainLock registryFile
     registry <- readRegistry
     let passwords = lines registry
     if candidate `elem` passwords 
       then do releaseLock registryFile
	       inventKey' len chars
       else do appendFile registryFile (candidate ++ "\n")
	       releaseLock registryFile
	       return candidate

readRegistry :: IO String
readRegistry =
  let registryPath = init registryDir in
  do assertDirectoryExists registryPath (return ())
     readFileNonExistent registryFile ""
