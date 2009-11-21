{-# OPTIONS_GHC -XPatternGuards -XFlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module ViewDoc (toggleSaveState, colorSaved) where

import Control.Monad
import qualified Data.ByteString.Char8 as Str
import XMonad.Util.ExtensibleState as XS
import qualified Data.Set as S 

import System.Posix.Types
import XMonad
import XMonad.Core
import XMonad.Hooks.ManageHelpers
import XMonad.Operations

history :: String
history = ".viewedDocs"

data Storage = Storage (S.Set ProcessID) deriving Typeable
instance ExtensionClass Storage where
   initialValue = Storage S.empty
unStorage (Storage s) = s

toggleSaveState :: X ()
toggleSaveState = withFocused (runQuery pid >=> updateDoc)

updateDoc :: Maybe ProcessID -> X ()
updateDoc t = case t of
    Nothing -> return ()
    Just p ->  do 
      x <- io $ Str.readFile history
      let y = map (read :: String -> [String]) $ filter (/= "") $ lines $ Str.unpack x
      let z = map (togglePid p) y
      colorWindows p (any (\ [_,_,pid,flag] -> pid == show p && read flag == True) z)
      io $ writeFile history (unlines (map show z))

togglePid p l@[cmd,args,pid,flag] 
    | show p == pid = [cmd,args,pid,show $ not (read flag :: Bool)]
    | otherwise = l
togglePid p l@[cmd,args,pid] = [cmd,args,pid,show False]
togglePid p l@[cmd,args] = [cmd,args,"0",show False]

colorWindows :: ProcessID -> Bool -> X ()
colorWindows p True  = do 
  XS.modify (push p)
  withFocused $ \w -> setWindowBorder' "blue" w
    where push p (Storage s) = Storage $ S.insert p s
colorWindows p False = do
  XS.modify (pull p)
  withFocused $ \w -> setWindowBorder' "green" w
    where pull p (Storage s) = Storage $ S.delete p s 

setWindowBorder' :: (MonadIO m, MonadReader XConf m) => String -> Window -> m ()
setWindowBorder' c w = do
    XConf { display = d } <- ask
    ~(Just pc) <- io $ initColor d c
    io $ setWindowBorder d w pc

colorSaved :: X ()
colorSaved = withFocused (runQuery pid >=> colorSaved')
  where colorSaved' mp | Just p  <- mp = colorSaved'' p
                       | Nothing <- mp = return ()
        colorSaved'' p = do 
          pids  <- XS.get
          case S.member p (unStorage pids) of
            True -> withFocused $ \w -> setWindowBorder' "blue" w
            False -> return ()
