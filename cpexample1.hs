-- arch-tag: ConfigParser example 1 to integrate into docs
import MissingH.ConfigParser
import Control.Monad.Error

main = do
          rv <- runErrorT $
              do
              cp <- join $ liftIO $ readfile empty "/etc/passwd"
              let x = cp
              liftIO $ putStrLn "In the test"
              nb <- get x "DEFAULT" "nobody"
              liftIO $ putStrLn nb
              foo <- get x "DEFAULT" "foo"
              liftIO $ putStrLn foo
              return "done"
          print rv
