{-# LANGUAGE FlexibleContexts #-}

module ShakeRules
  where

import Control.Monad (void)
import Development.Shake
import Development.Shake.FilePath
import System.Process

genTagShell :: String -> String
genTagShell path = "export LC_ALL=C; cd " ++ path ++ "; find -type f | egrep \\.hs$\\|\\.lhs$ | xargs -Ii hothasktags i | sort > tags 2>/dev/null"

genTagFile :: String -> Action ()
genTagFile path = void $ liftIO (system $ genTagShell path)

buildWants :: String -> String -> Rules ()
buildWants base x = want [base ++ "/" ++ x ++ "/tags"]

buildTagRule :: String -> Rules ()
buildTagRule base = do
    (base ++ "/*/tags") *> \out -> do
        let packageDir = takeDirectory out
        need [packageDir]
        genTagFile packageDir

fetchArchiveRule :: String -> Rules ()
fetchArchiveRule base = do
    (base ++ "/*") *> \out -> do
        let archive = out `addExtension` "tar.gz"
            packageUrl = "https://hackage.haskell.org/package/" 
                            ++ archive
            wd = takeDirectory out
        unit $ cmd (Cwd wd) "wget" [packageUrl]
        unit $ cmd (Cwd base) "tar -xzf" [archive]
        unit $ cmd (Cwd base) "rm" [archive]

buildThisTagRule :: Rules ()
buildThisTagRule = do
    want ["tags"]
    "tags" *> \out -> do
        genTagFile "."

unit :: Action () -> Action ()
unit x = x
