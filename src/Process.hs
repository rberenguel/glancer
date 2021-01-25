{-# LANGUAGE OverloadedStrings #-}

module Process where

import Control.Monad (replicateM)
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import System.Directory (getTemporaryDirectory)
import System.IO
  ( hPutStrLn,
    stderr,
  )
import System.Process (callCommand, callProcess, readProcess)
import System.Random (Random (randomRIO))

newtype Url = Url T.Text

newtype Title = Title T.Text

newtype Id = Id T.Text

newtype Dir = Dir T.Text

newtype Filename = Filename T.Text

data Video = Video
  { url :: Url,
    title :: Title,
    file :: Filename
  }

getTitle :: Url -> IO Title
getTitle (Url url) = do
  title <- readProcess "youtube-dlc" ["-e", "--no-warnings", "--no-playlist", T.unpack url] []
  return (Title $ TE.decodeUtf8 $ B.pack title) -- This deletes badly encoded characters, which is better than having them but worse than properly encoding them. Help appreciated

getId :: Url -> IO Id
getId (Url url) = do
  id <- readProcess "youtube-dlc" ["--get-id", "--no-warnings", "--no-playlist", T.unpack url] []
  return (Id $ T.pack id)

youtubeURL :: Id -> Url
youtubeURL (Id id) = Url ("https://www.youtube.com/watch?v=" <> id)

generateVideo :: Video -> Dir -> IO ()
generateVideo (Video (Url url) _ (Filename videoName)) (Dir dir) = do
  callProcess "youtube-dlc" ["-q", "--no-playlist", "-f mp4", T.unpack ("-o" <> dir <> "/" <> videoName <> ".mp4"), "--sub-langs", "en", "--write-auto-sub", "--write-sub", "--no-warnings", "--no-cache-dir", T.unpack url]

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
  hPutStrLn stderr $ T.unpack ("The video is titled '" <> T.strip (coerce title) <> "'")
  id <- getId url
  let yourl = youtubeURL id
  hPutStrLn stderr $ T.unpack (T.strip ("Seems like the video is in " <> coerce yourl))
  let video = Video yourl title videoName
  generateVideo video dir
  hPutStrLn stderr (T.unpack ("Downloaded video to " <> coerce dir <> coerce videoName <> "(.mp4|en.vtt)"))
  hPutStrLn stderr "Generating still images from video (this may take a while)"
  generateShots dir videoName
  hPutStrLn stderr "Generated images"
  deleteVideo dir videoName
  return (dir, video)
