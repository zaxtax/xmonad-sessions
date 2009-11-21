module Main where

import System.Posix.Process
import System.Directory
import System.Path
import qualified Data.ByteString.Char8 as B

history :: String
history = ".viewedDocs"

unJust (Just a) = a
saveFile h = unJust $ absNormPath h history

launch prog args = forkProcess $ executeFile ("/usr/bin/" ++ prog) True args Nothing

main = do
  home <- getHomeDirectory
  f <- B.readFile (saveFile home)
  g <- mapM launchFile (lines $ B.unpack f)
  writeFile history (unlines g)

launchFile :: String -> IO String
launchFile "" = return ""
launchFile f  = launchFile' (read f)
  where launchFile' :: [String] -> IO String
        launchFile' [cmd,args,pid,flag] 
            | read flag == True = do 
                pid <- launch cmd [args]
                return $ show [cmd,args,show pid,flag]
            | otherwise = return $ show [cmd,args,pid,flag]
        launchFile' l = return $ show l