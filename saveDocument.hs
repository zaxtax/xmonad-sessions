module Main where

import System.Environment
import System.Posix.Process
import System.Path
import System.Directory

unJust (Just a) = a

main = do
  home <- getHomeDirectory
  name <- getProgName
  args <- getArgs
  pid  <- launch name args
  appendFile (unJust $ absNormPath home saveFile) 
             ((show $ name:args ++ [show pid,show False]) ++ "\n")

saveFile = ".viewedDocs"

launch prog args = forkProcess $ executeFile ("/usr/bin/" ++ prog) True args Nothing