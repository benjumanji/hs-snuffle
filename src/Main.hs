module Main (main) where

import Data.Char (isSpace)
import Data.List
import Data.Monoid ((<>))
import Development.Shake
import Control.Applicative
import System.Process

import ShakeRules

main :: IO ()
main = do
    home <- getHomeDirectory
    db <- getPackageDbPath
    packageList <-  maybe (return []) getPackageList db
    putStr $ intercalate "," packageList
    shake shakeOptions $ do
        mapM_ (buildTagRule "/home/ben/src/haskell") packageList

getPackageDbPath :: IO (Maybe String)
getPackageDbPath = getPath <$> io
  where 
    io = readProcess "grep" ["package-db", "cabal.sandbox.config"] ""
    getPath = stripPrefix "package-db: "

getPackageList :: String -> IO [String]
getPackageList x = toList <$> io
  where
    clean = strip x 
    io = readProcess "ghc-pkg" ["list","--package-db", clean] ""
    toList = init . tail . map stripStart . lines

strip :: String -> String
strip = stripStart . takeWhile (not isSpace)

stripStart :: String -> String
stripStart = dropWhile (isSpace) 
