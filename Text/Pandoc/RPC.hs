module Text.Pandoc.RPC where

import Text.Pandoc
import Text.Pandoc.Rpc.Protocol
import Text.Pandoc.Rpc.Protocol.PandocRequest
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.WireMessage
import qualified Text.Pandoc.Rpc.Protocol.PandocResponse as R
import Data.ByteString.Lazy.Char8 hiding (putStrLn)
import qualified Data.ByteString.Lazy as L
import System.ZMQ3
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_)

pipeline :: String -> String -> Either String (ReaderOptions -> String -> IO Pandoc, Writer)
pipeline inputFmt outputFmt = 
  case getReader inputFmt of
    Left error -> Left error
    Right reader@_ -> writer reader
  where writer reader =
          case getWriter outputFmt of
            Left error -> Left error
            Right writer@_ -> Right (reader, writer)
            
write :: Writer -> Pandoc -> IO String
write writer native = case writer of 
  PureStringWriter writer' -> return $ writer' def native  
  IOStringWriter writer' -> writer' def native
  IOByteStringWriter writer' -> do 
    out <- writer' def native  
    return $ unpack out
        
transform :: (ReaderOptions -> String -> IO Pandoc) -> Writer -> String -> IO String
transform reader writer input = do
  native <- reader def input
  write writer native

pandoc :: String -> String -> String -> IO (Either String String)
pandoc inputFmt outputFmt input = do
  case pipeline inputFmt outputFmt of
    Left error -> return $ Left error
    Right (reader, writer) -> do
      out <- transform reader writer input
      return $ Right out
      
pandoc' :: PandocRequest -> IO R.PandocResponse
pandoc' req = do
  resp <- pandoc (uToString $ inputFmt req) (uToString $ outputFmt req) (uToString $ input req)
  return $ case resp of 
    Left error -> R.PandocResponse {
      R.error = Just $ uFromString error,
      R.output = Nothing }
    Right output -> R.PandocResponse {
      R.error = Nothing,
      R.output = Just $ uFromString output }
                    
worker :: (Receiver a, Sender a) => Socket a -> IO ()
worker socket = do
  msg <- receive socket
  resp <- case messageGet $ fromStrict msg of
        Left error -> return $ R.PandocResponse { R.error = Just $ uFromString error, R.output = Nothing }
        Right (msg, _) -> pandoc' msg
  send socket [] $ toStrict $ messagePut resp

workers :: Int -> Context -> IO ()
workers workerCount context = forM_ [0..workerCount] $ \_ -> forkIO $ do
    withSocket context Rep $ \responder -> do
      connect responder "inproc://pandoc"
      forever $ worker responder

main :: String -> Int -> IO ()
main endpoint workerCount = do
  withContext $ \context -> do
    withSocket context Router $ \frontend -> do
      bind frontend endpoint
      withSocket context Dealer $ \backend -> do
        bind backend "inproc://pandoc"
        workers workerCount context
        proxy frontend backend Nothing