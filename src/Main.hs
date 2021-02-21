{-# LANGUAGE OverloadedStrings #-}

module Main where

import Captions (convertToHTML)
import Data.Text (pack)
import qualified Data.Text as T
import GHC.IO.Encoding.Latin1 (ascii)
import qualified Options.Applicative as Ap
import Parser (subsP)
import Process
  ( Dir (..), 
  Filename (..),
    Url (..),
    Video (..),
    processURL,
  )
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath, splitPath, (-<.>), (</>))
import System.IO
  ( IOMode (ReadMode),
    hGetContents,
    hPutStrLn,
    hSetEncoding,
    openFile,
    stderr,
  )
import Text.Megaparsec (parse)
import Prelude
import Data.Coerce (coerce)

data CLIConfig = CLIConfig
  { _url :: String,
    _filename :: String
  }

cliConfig :: Ap.Parser CLIConfig
cliConfig =
  CLIConfig
    <$> Ap.strArgument (Ap.metavar "URL" <> Ap.help "Youtube URL")
    <*> Ap.strArgument (Ap.metavar "FILEPATH" <> Ap.help "HTML file name (don't add extension)")

getFullPath :: FilePath -> IO FilePath
getFullPath s = case splitPath s of
  "~/" : t -> joinPath . (: t) <$> getHomeDirectory
  _ -> return s

start :: CLIConfig -> IO ()
start (CLIConfig url filename) = do
  hPutStrLn stderr ("Looking for video in " ++ url)
  (dir_, video) <- processURL (Url $ T.pack url)
  let videoName = coerce (file video)
  let dir = coerce dir_
  capsPath <- getFullPath (dir </> videoName -<.> "en.vtt")
  handle <- openFile capsPath ReadMode
  hSetEncoding handle ascii
  contents <- hGetContents handle
  let parsed = parse subsP "" (pack contents)
  html <- convertToHTML video dir_ parsed
  destinationPath <- getFullPath (filename -<.> "html")
  hPutStrLn stderr ("Writing html to " ++ destinationPath)
  writeFile destinationPath (T.unpack html)
  hPutStrLn stderr ("Data written to " ++ destinationPath)

main :: IO ()
main = do
  Main.start =<< Ap.execParser opts
  where
    opts =
      Ap.info
        (cliConfig Ap.<**> Ap.helper)
        ( Ap.fullDesc
            <> Ap.progDesc "Glancer"
            <> Ap.header "Why not"
        )
