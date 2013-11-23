module ShakeRules
  where

import Development.Shake
import Development.Shake.FilePath

buildTagRule :: Rules ()
buildTagRule base x = do
    want [base ++ "/" ++ x ++ "/tags"]
    "//tags" *> \out -> do
        let packagedir = dropDirectory1 out
            archive = packageDir `addExtension` "tar.gz"
        need [archive]
        cmd (Cwd base) "tar -xzf" [archive]
        cmd (Cwd packageDir) "export LC_ALL=C; find -type f | egrep \\.hs$\\|\\.lhs$ | xargs -Ii hothasktags i | sort > tags 2>/dev/null" 

fetchArchiveRule = do
    "//*.tar.gz" *> \out -> do
        let packageUrl = "https://hackage.haskell.org/package/" 
                            + (takeDirectory1 out)
            wd = dropDirectory1 out
        cmd (Cwd base) "wget" [archive]
