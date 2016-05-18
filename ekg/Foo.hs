{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Remote.Monitoring

main = do
  ekg <- forkServer "localhost" 8000
  putStrLn "ss"
  forever $ getLine >>= putStrLn
