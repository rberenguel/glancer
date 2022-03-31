{-# LANGUAGE OverloadedStrings #-}

module Process where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as B
import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (getTemporaryDirectory)
import System.FilePath ((-<.>), (</>))
import System.IO
  ( hPutStrLn,
    stderr,
  )
import System.Process (callCommand, callProcess, readProcess)
import System.Random (Random (randomRIO))

newtype Url = Url T.Text

newtype Title = Title T.Text

newtype Id = Id T.Text

newtype Dir = Dir String

newtype Filename = Filename String

data Video = Video
  { url :: Url,
    title :: Title,
    file :: Filename
  }

getTitle :: Url -> IO Title
getTitle (Url url) = do
  title <- readProcess "yt-dlp" ["-e", "--no-warnings", "--no-playlist", T.unpack url] []
  return (Title $ TE.decodeUtf8 $ B.pack title) -- This deletes badly encoded characters, which is better than having them but worse than properly encoding them. Help appreciated

getId :: Url -> IO Id
getId (Url url) = do
  id <- readProcess "yt-dlp" ["--get-id", "--no-warnings", "--no-playlist", T.unpack url] []
  return (Id $ T.pack id)

youtubeURL :: Id -> Url
youtubeURL (Id id) = Url ("https://www.youtube.com/watch?v=" <> id)

generateVideo :: Video -> Dir -> IO ()
generateVideo (Video (Url url) _ (Filename videoName)) (Dir dir) = do
  callProcess command arguments
  where
    command = "yt-dlp"
    arguments = ["-q", "--no-playlist", "-f mp4", coerce ("-o" <> dir </> videoName -<.> "mp4"), "--sub-langs", "en", "--write-auto-sub", "--write-sub", "--no-warnings", "-k", "--no-cache-dir", T.unpack url]

args :: Dir -> Filename -> [String] -> String -> [String]
args (Dir dir) (Filename videoName) selector suffix = [ "-i",
                            coerce (dir </> videoName -<.> "mp4")]
                            ++ selector ++ [
                            coerce (dir </> "glancer-img" <> suffix -<.> "jpg"),
                            "-hide_banner",
                            "-loglevel",
                            "panic"
                          ]

generateShots :: Dir -> Filename -> IO ()
generateShots dir video = do
  callProcess "ffmpeg" (args dir video ["-vf", "fps=1/30"] "%04d")
  callProcess "ffmpeg" (args dir video ["-vframes",  "1", "-ss", "3"] "0000" )

deleteVideo :: Dir -> Filename -> IO ()
deleteVideo (Dir dir) (Filename videoName) = callProcess "rm" [coerce (dir </> videoName -<.> "mp4")]

deleteImages :: Dir -> IO ()
deleteImages (Dir dir) = callCommand $ T.unpack ("rm " <> T.pack (dir </> "glancer-img*"))

processURL :: Url -> IO (Dir, Video)
processURL url = do
  dir <- Dir <$> getTemporaryDirectory
  videoName <- Filename <$> replicateM 10 (randomRIO ('a', 'z'))
  title <- getTitle url
  hPutStrLn stderr $ T.unpack ("The video is titled '" <> T.strip (coerce title) <> "'")
  id <- getId url
  let yourl = youtubeURL id
  hPutStrLn stderr $ T.unpack (T.strip ("Seems like the video is in " <> coerce yourl))
  let video = Video yourl title videoName
  hPutStrLn stderr "Downloading video (this may take a while)"
  generateVideo video dir
  hPutStrLn stderr (T.unpack ("Downloaded video to " <> (T.pack . coerce) dir <> (T.pack . coerce) videoName <> "(.mp4|en.vtt)"))
  hPutStrLn stderr "Generating still images from video (this may take a while)"
  generateShots dir videoName
  hPutStrLn stderr "Generated images"
  deleteVideo dir videoName
  return (dir, video)
