{- arch-tag: Generic Dict-Like Object Support
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module     : MissingH.AnyDBM
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen,
   Maintainer : jgoerzen@complete.org
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

This module provides a generic infrastructure for supporting storage of
hash-like items with String -> String mappings.  It can be used for in-memory
or on-disk items.

-}
module MissingH.AnyDBM (-- * The AnyDBM class
                        AnyDBM(..),
                        -- * AnyDBM utilities
                        mapA,
                        strFromA, strToA
                       )
where
import Prelude hiding (lookup)
import System.IO
import Data.HashTable
import Control.Exception
import MissingH.List(strFromAL, strToAL)

{- | The main class for items implementing this interface.

People implementing this class should provide methods for:

* 'closeA' (unless you have no persistent storage)

* 'flushA' (unless you have no persistent storage)

* 'insertA'

* 'deleteA'

* 'lookupA'

* either 'toListA' or 'keysA'
-}
class AnyDBM a where
    {- | Close the object, writing out any unsaved data to disk if necessary.

         If you implement this, make sure your implementation calls 'flushA'.

         Note: if you have an object opened for writing, you MUST
         call closeA on it when you are done.  Implementations are not
         required to preserve your data otherwise.
       -}
    closeA :: a -> IO ()

    {- | Flush the object, saving any un-saved data to disk but not closing
         it. Called automatically by 'closeA'. -}
    flushA :: a -> IO ()

    {- | Insert the given data into the map. Existing data with the same key
       will be overwritten. -}
    insertA :: a             -- ^ AnyDBM object
            -> String           -- ^ Key
            -> String        -- ^ Value
               -> IO ()

    {- | Delete the data referenced by the given key.  It is not an error
         if the key does not exist. -}
    deleteA :: a -> String -> IO ()

    {- | True if the given key is present. -}
    hasKeyA :: a -> String -> IO Bool

    {- | Find the data referenced by the given key. -}
    lookupA :: a -> String -> IO (Maybe String)

    {- | Look up the data and raise an exception if the key does not exist.
         The exception raised is PatternMatchFail, and the string accompanying
         it is the key that was looked up.-}
    forceLookupA :: a -> String -> IO String

    {- | Call 'insertA' on each pair in the given association list, adding
       them to the map. -}
    insertListA :: a -> [(String, String)] -> IO ()

    {- | Return a representation of the content of the map as a list. -}
    toListA :: a -> IO [(String, String)]

    {- | Returns a list of keys in the 'AnyDBM' object. -}
    keysA :: a -> IO [String]

    {- | Returns a list of values in the 'AnyDBM' object. -}
    valuesA :: a -> IO [String]

    valuesA h = do l <- toListA h
                   return $ map snd l

    keysA h = do l <- toListA h
                 return $ map fst l


    toListA h = 
        let conv k = do v <- forceLookupA h k
                        return (k, v)
            in do k <- keysA h
                  mapM conv k

    forceLookupA h key = 
        do x <- lookupA h key
           case x of 
                  Just y -> return y
                  Nothing -> throwIO $ PatternMatchFail key
        
    insertListA h [] = return ()
    insertListA h ((key, val):xs) = do insertA h key val
                                       insertListA h xs

    hasKeyA h k = do l <- lookupA h k
                     case l of
                            Nothing -> return False
                            Just _ -> return True

    closeA h = flushA h

    flushA h = return ()
                  
{- | Similar to MapM, but for 'AnyDBM' objects. -}
mapA :: AnyDBM a => a -> ((String, String) -> IO b) -> IO [b]
mapA h func = do l <- toListA h
                 mapM func l

{- | Similar to 'MissingH.List.strToAL' -- load a string representation
into the AnyDBM.  You must supply an existing AnyDBM object;
the items loaded from the string will be added to it. -}
strToA :: AnyDBM a => a -> String -> IO ()
strToA h s = insertListA h (strToAL s)

{- | Similar to 'MissingH.List.strFromAL' -- get a string representation of
the entire AnyDBM. -}
strFromA :: AnyDBM a => a -> IO String
strFromA h = do l <- toListA h 
                return (strFromAL l)

instance AnyDBM (HashTable String String) where
    insertA h k v = do delete h k
                       insert h k v
    deleteA = delete
    lookupA = lookup
    toListA = toList
    
