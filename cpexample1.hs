-- arch-tag: ConfigParser example 1 to integrate into docs
import MissingH.ConfigParser
import Control.Monad.Error

main = do
          --let d = readfile empty "/etc/passwd"
          rv <- runErrorT $
              do
              cp <- liftIO $ readfile empty "/etc/passwd"
              x <- cp
              liftIO $ putStrLn "In the test"
              --cp <- d
              --liftIO $ print (sections cp)
              nb <- get x "DEFAULT" "nobody"
              liftIO $ putStrLn nb
              foo <- get x "DEFAULT" "foo"
              liftIO $ putStrLn foo
              return "done"
          print rv
