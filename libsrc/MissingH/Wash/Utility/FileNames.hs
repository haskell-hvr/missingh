module MissingH.Wash.Utility.FileNames where

longestSuffix :: (a -> Bool) -> [a] -> [a]
longestSuffix p xs =
  let f [] suffix = suffix
      f (x : xs) suffix = f xs (if p x then xs else suffix)
  in  f xs xs


-- |longest suffix of path that does not contain '/'
filePart :: String -> String
filePart =
  longestSuffix (=='/')

-- |longest suffix of path that does not contain '.'
extName :: String -> String
extName =
  longestSuffix (=='.')

-- |longest prefix so that the rest contains '.'; entire string if no '.' present
baseName :: String -> String
baseName filename =
  let f "" = ""
      f ('.':rest) = g rest rest
      f (x:rest) = x:f rest
      g "" lst = ""
      g ('.':rest) lst = '.':f lst
      g (x:rest) lst = g rest lst
  in  f filename

-- |splits input at each '/'
fileToPath :: String -> [String]
fileToPath filename =
  let f acc path "" = reverse (reverse acc: path)
      f acc path ('/':xs) = f "" (reverse acc: path) xs
      f acc path (x:xs) = f (x:acc) path xs
  in  f "" [] filename

-- |drop the last component of a file path
dropLastComponent :: String -> String
dropLastComponent path =
  let f "" = ""
      f rpath = g rpath
      g ('/':rest) = g rest
      g "" = "/"
      g rpath = dropWhile (/='/') rpath
  in reverse (f (reverse path))
