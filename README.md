# xmonad-sessions

xmonad-sessions is a way to use xmonad to control which of your applications 
persist and are easily restored.

## Installing

place ViewDoc.hs into ~/.xmonad/lib/

````bash
$ cp ViewDoc.hs ~/.xmonad/lib
````

compile saveDocument and loadDocuments and place in path

````bash
$ ghc --make saveDocument
$ mv saveDocument /home/user/bin
````

symlink all applications you want session saved
````bash
$ ln -s /usr/user/bin/saveDocument /home/user/bin/gv
$ ln -s /usr/user/bin/saveDocument /home/user/bin/evince
````

modify xmonad.hs

````haskell
import XMonad
import qualified Data.Map as M
import ViewDoc

myLogHook :: X ()
myLogHook = do ewmhDesktopsLogHook
               colorSaved

mykeys (XConfig {modMask = modm}) = M.fromList $
   [  ((modm, xK_s), toggleSaveState)
   ,  ((modm .|. shiftMask, xK_s), launchDocuments)
   ]

main = xmonad $ defaultConfig 
       { loghook = myLogHook
       , keys    = (\c -> mykeys c `M.union` keys defaultConfig c)
       }
````



## Using

If you want to save any window focus over it and Mod-s.
Mod-s again will untoggle it

To restore all previous windows saved Mod-S

All applications that have been wrapped with saveDocument will be restorable
