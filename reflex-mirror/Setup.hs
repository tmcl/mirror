import Distribution.Simple
import System.Process (callCommand)
import System.Directory (doesPathExist)
import Control.Monad (when)

main = defaultMainWithHooks myHooks

myHooks = simpleUserHooks {
  buildHook = \ a b c d -> do
    windows <- doesPathExist "C:\\"
    when windows $ 
       callCommand "C:\\programdata\\chocolatey\\lib\\ghc\\tools\\ghc-8.8.3\\mingw\\bin\\windres.exe -i example-resources\\win32.rc example-resources\\res.o"
    buildHook simpleUserHooks a b c d
}


