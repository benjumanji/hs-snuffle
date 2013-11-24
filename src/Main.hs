module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Char (isSpace)
import Data.List
import Data.Monoid ((<>))
import Development.Shake
import Development.Shake.FilePath
import Options.Applicative
import System.Process

import ShakeRules

data Snuffle = Snuffle
    { buildDeps :: Bool   -- ^ should build tags for dependencies
    , buildOwn  :: Bool   -- ^ should build tags for own source
    , pathDeps  :: String -- ^ path to folder for dependent source
    }

snuffle :: Parser Snuffle
snuffle = Snuffle
    <$> switch
        ( long "build-deps"
       <> short 'd'
       <> value False
       <> help "should snuffle build a tags file for each cabal dependency"
        )
    <*> switch
        ( long "build-own"
       <> short 'o'
       <> value True
       <> help "should snuffle build a tags file for the project source"
        )
    <*> strOption
        ( long "src-path"
       <> short 'p'
       <> value "~/.hs-snuffle/share/src"
       <> metavar "PATH"
       <> help "where should snuffle store fetched source files"
        )

opts = info (helper <*> snuffle) desc
  where
    desc = fullDesc <> pDesc <> pHeader
    pDesc = progDesc $ "Fetch sources and generate tags for cabal sandboxed"
                        ++ " dependencies. Run in the same directory as any"
                        ++ " sandboxed cabal project to generate tags for "
                        ++ "your project source as well as that of any "
                        ++ "dependent packages"
    pHeader = header "Snuffle - autogen vim tags for cabal projects!"

-- | unpack snuffle
withSnuffle :: (Bool -> Bool -> FilePath -> IO ()) -- ^ run snuffle options
            -> Snuffle                             -- ^ configured options
            -> IO ()
withSnuffle f (Snuffle deps own path) = f deps own path

main :: IO ()
main = execParser opts >>= withSnuffle go
  where
    go deps own path = do
        when own $ shake shakeOptions buildThisTagRule
        when deps $ do
            db <- getPackageDbPath
            packageList <- maybe (return []) getPackageList db
            putStr $ intercalate "," packageList
            let shakeDeps = shakeOptions { shakeFiles = path </> ".shake" }
            shake shakeDeps $ do
                fetchArchiveRule path
                buildTagRule path
                buildWants path packageList

-- | get the path to the sandbox package db
getPackageDbPath :: IO (Maybe String)
getPackageDbPath = getPath <$> io
  where 
    io = readProcess "grep" ["package-db", "cabal.sandbox.config"] ""
    getPath = stripPrefix "package-db: "

-- | get the list of install packages
getPackageList :: String -> IO [String]
getPackageList x = toList <$> io
  where
    clean = strip x 
    io = readProcess "ghc-pkg" ["list","--package-db", clean] ""
    toList = init . tail . map stripStart . lines

-- | leading and trailing whitespace from a string
strip :: String -> String
strip = stripStart . takeWhile (not . isSpace)

-- | string leading whitespace from a string
stripStart :: String -> String
stripStart = dropWhile isSpace
