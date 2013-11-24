{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module ShakeRules
  where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Development.Shake
import Development.Shake.FilePath

-- | Shell command for generating an unsorted tags file. The sorting is
--   split from the tags generation to avoid problems with hGetContents and
--   invalid byte sequences.
genTagShell :: String
genTagShell = "find -type f | egrep \\.hs$\\|\\.lhs$ | xargs -Ii hothasktags i > tags.unsorted"

-- | sorts the tags file. Must be run with "LC_ALL=C" set in the env.
sortTagShell :: String
sortTagShell = "sort tags.unsorted > tags"

-- | generate a rule to say that we want a set of tag files.
buildWants :: String   -- ^ base directory for our source files
           -> [String] -- ^ list of packages with version numbers
           -> Rules ()
buildWants base = want . map f
  where
    f x = base </> x </> "tags"

-- | this rule matches all tag files for a versioned pacakge within our
--   source directory. Given that the source dir exists, make a tags file
buildTagRule :: String   -- ^ base directory for source files
             -> Rules ()
buildTagRule base = do
    (base ++ "/*/tags") *> \out -> do
        let packageDir = takeDirectory out
        need [packageDir]
        genTagFile packageDir

-- | Create a source directory by fetching it from hackage and unpacking
--   it.
fetchArchiveRule :: String -> Rules ()
fetchArchiveRule base = do
    (base ++ "/*") *> \out -> do
        let archive = takeFileName out `addExtension` "tar.gz"
            packageUrl = "https://hackage.haskell.org/package/" 
                            ++ archive
            wd = out
            g cmd' file = u $ cmd (Cwd base) cmd' [file]
        g "wget" packageUrl
        g "tar -zxf" archive
        g "rm" archive

-- | In the case of our own source files (non-remote) we want to regen the
--   tag file as soon as any of the sources files change
getSourceFiles :: Action [String]
getSourceFiles = (lines . fromStdout) <$> cmd Shell shellCmd
  where
    shellCmd = "find -type f | egrep \\.hs$\\|\\.lhs$"

-- | build a tags file for the current directory if it doesn't already
--   exist
buildThisTagRule :: Rules ()
buildThisTagRule = do
    want ["tags"]
    "tags" *> \_ -> do
        need =<< getSourceFiles
        genTagFile "."

-- | generate a tag file for a given directory 
genTagFile :: String -> Action ()
genTagFile wd = do
    let cwd = Cwd wd
        env = Env [("LC_ALL", "C")]
    u $ cmd Shell cwd genTagShell
    u $ cmd Shell cwd env sortTagShell
    cmd cwd "rm" ["tags.unsorted"]

-- | force a return type for cmd.
u :: Action () -> Action ()
u = id
