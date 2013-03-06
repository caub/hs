{-# LANGUAGE OverloadedStrings #-}

import Network
import System.Directory
import System.IO
import Data.List (sort)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B_

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A hiding (accept)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Control.Concurrent
import Debug.Trace


absRoot :: B.ByteString
absRoot = "/home/cab/Documents"

main :: IO ()
main = withSocketsDo $ do
    s <- listenOn (PortNumber 8080)
    trash <- newEmptyMVar
    forkIO $ gc trash
    loop' s trash

gc :: MVar ThreadId -> IO () -- garbage collector
gc trash = do
    thread <- takeMVar trash
    killThread thread
    traceIO $ "gc: "++ show thread
    gc trash

loop' :: Socket -> MVar ThreadId -> IO ()
loop' s trash = (next s trash `catch` \e -> do
                        traceIO $ show e
                        return ()
           ) >> loop' s trash

next :: Socket -> MVar ThreadId -> IO ()
next s trash = do
    (h, _, _) <- accept s
    
    forkIO $ do
        B.hGetContents h >>= route >>= B.hPutStr h
        hClose h
        myThreadId >>= putMVar trash
    return ()

route :: B.ByteString -> IO B.ByteString
route req =
    case B.split 32 req of
        ("GET" : path : _) -> deliver path
        _ -> complain "405 Not Supported"

deliver :: B.ByteString -> IO B.ByteString
deliver path = do
    let absPath = B_.unpack (absRoot <> path); path_ = B_.unpack path
    isFile <- doesFileExist absPath
    if isFile
        then transfer path absPath `catch` (\_ -> complain "500 Server Error")
        else do isDir <- doesDirectoryExist absPath
                if isDir
                    then fmap (response "200 OK" ["Content-Type: text/html"] . renderHtml . dirsT path_ . sort) (getDirectoryContents absPath)
                    else complain "404 File Not Found"

transfer :: B.ByteString -> String -> IO B.ByteString
transfer path absPath = do
   body <- B.hGetContents =<< openBinaryFile absPath ReadMode
   return $ response "200 OK" ["Content-Type: " <> guessType path] body

response :: B.ByteString -> [B.ByteString] -> B.ByteString -> B.ByteString
response status headers body =
  B.concat [B_.unlines (("HTTP/1.1 " <> status) : headers), "\n", body]

complain :: B.ByteString -> IO B.ByteString
complain status = return $ response status [] status

guessType :: B.ByteString -> B.ByteString
guessType path
    | ".html" `B.isSuffixOf` path    = "text/html"
    | [".txt"
      ,".hs"
      ,".c"
      ]`fany` (`B.isSuffixOf` path)  = "text/plain"
    | ".css"  `B.isSuffixOf` path    = "text/css"
    | ".js"   `B.isSuffixOf` path    = "text/javascript"
    | otherwise                      = "application/octet-stream"
  where fany = flip any




-------------- Templates using Blaze---------------

dirsT :: String -> [String] -> Html
dirsT root items = docTypeHtml $ do
    H.head $
        H.title "BlazeHtml benchmarks"
    body ! A.style "width: 500px; margin: 0px auto;" $
        dt $ do
            dl >> h1 $ toMarkup root
            mapM_ (dirItemT root) items

dirItemT :: String -> String -> Html
dirItemT root item = dd $
    a ! href (toValue $ prefix++item) $ toMarkup item
  where 
    prefix = if root=="/" then "" else root++"/"

