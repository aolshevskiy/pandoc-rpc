module Main where

import qualified Text.Pandoc.RPC as RPC
import System.Console.GetOpt
import System.Environment(getArgs)

data Options = Options { 
  endpoint :: String,
  workers :: Int 
  } deriving Show
           
defaultOptions = Options {
  endpoint = "tcp://*:5559",
  workers = 10  
  }

opts = [
  Option ['e'] ["endpoint"] 
  (ReqArg (\ e opts -> opts { endpoint = e }) "string") 
  "endpoint in zeromq format e.g. tcp://*:5559",
  Option ['w'] ["workers"] 
  (ReqArg (\ w opts -> opts { workers = read w }) "int") 
  "workers count"
  ]
       
parseOpts argv =
  case getOpt Permute opts argv of
    (o,n,[]) -> Right $ foldl (flip id) defaultOptions o
    (_,_,errs) -> Left $ (concat errs ++ usageInfo header opts)
  where header = "Usage: pandoc-rpc [OPTION...]"
  
main :: IO ()
main = do
  argv <- getArgs
  case parseOpts argv of
    Left err -> putStr err
    Right opts -> RPC.main (endpoint opts) (workers opts)
  