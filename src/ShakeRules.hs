{-# LANGUAGE FlexibleContexts #-}

module ShakeRules
  where

import Development.Shake
import Development.Shake.FilePath
import System.Process

genTags :: String -> String
genTags path = "export LC_ALL=C; cd " ++ path ++ "; find -type f | egrep \\.hs$\\|\\.lhs$ | xargs -Ii hothasktags i | sort > tags 2>/dev/null"

buildWants :: String -> String -> Rules ()
buildWants base x = want [base ++ "/" ++ x ++ "/tags"]

buildTagRule :: String -> Rules ()
buildTagRule base = do
    (base ++ "/*/tags") *> \out -> do
        let packageDir = takeDirectory out
            archive = packageDir 
        need [packageDir]
        _ <- liftIO (system $ genTags packageDir)
        return ()

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
        return ()

buildThisTagRule :: Rules ()
buildThisTagRule = do
    want ["tags"]
    "tags" *> \out -> do
        _ <- liftIO (system $ genTags ".")
        return ()

unit :: Action () -> Action ()
unit x = x
