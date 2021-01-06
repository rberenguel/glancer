{-# LANGUAGE OverloadedStrings #-}

module Process where

import Control.Monad (replicateM)
import Data.Coerce (coerce)
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory)
import System.IO
  ( hPutStrLn,
    stderr,
  )
import System.Process (callCommand, callProcess, readProcess)
import System.Random (Random (randomRIO))

newtype Url = Url T.Text

newtype Title = Title T.Text

newtype Dir = Dir T.Text

newtype Filename = Filename T.Text

data Video = Video
  { url :: Url,
    title :: Title,
    file :: Filename
  }

getTitle :: Url -> IO Title
getTitle (Url url) = do
  title <- readProcess "youtube-dlc" ["-e", T.unpack url] []
  return (Title $ T.pack title)

generateVideo :: Video -> Dir -> IO ()
generateVideo (Video (Url url) _ (Filename videoName)) (Dir dir) = do
  callProcess "youtube-dlc" ["-q", "--no-playlist", "-f mp4", T.unpack ("-o" <> dir <> "/" <> videoName <> ".mp4"), "--write-auto-sub", "--no-cache-dir", T.unpack url]

generateShots :: Dir -> Filename -> IO ()
generateShots (Dir dir) (Filename videoName) = do
  callProcess
    "ffmpeg"
    [ "-i",
      T.unpack (dir <> "/" <> videoName <> ".mp4"),
      "-vf",
      "fps=1/30",
      T.unpack (dir <> "/glancer-img%04d.jpg"),
      "-hide_banner",
      "-loglevel",
      "panic"
    ]

deleteVideo :: Dir -> Filename -> IO ()
deleteVideo (Dir dir) (Filename videoName) = callProcess "rm" [T.unpack (dir <> videoName <> ".mp4")]

deleteImages :: Dir -> IO ()
deleteImages (Dir dir) = callCommand $ T.unpack ("rm " <> dir <> "glancer-img*")

processURL :: Url -> IO (Dir, Video)
processURL url = do
  dir <- Dir . T.pack <$> getTemporaryDirectory
  videoName <- Filename . T.pack <$> replicateM 10 (randomRIO ('a', 'z'))
  title <- getTitle url
  let video = Video url title videoName
  generateVideo video dir
  hPutStrLn stderr (T.unpack ("Downloaded video to " <> coerce dir <> coerce videoName <> "(.mp4|en.vtt)"))
  hPutStrLn stderr "Generating still images from video (this may take a while)"
  generateShots dir videoName
  hPutStrLn stderr "Generated images"
  deleteVideo dir videoName
  return (dir, Video url title videoName)
