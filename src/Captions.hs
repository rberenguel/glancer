{-# LANGUAGE OverloadedStrings #-}

module Captions where

import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.Sequence (fromList, mapWithIndex)
import Data.Text (intercalate, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Html
import Parser (Caption (block, txt), TimeBlock (..), TimeDef (..), inBlock, secs)
import Process
  ( Dir (..),
    Url (..),
    Video (..),
    deleteImages,
  )
import System.IO
  ( hPutStrLn,
    stderr,
  )
import qualified System.Process.ByteString as B
import Text.Printf (printf)

convertToHTML :: Show a => Video -> Dir -> Either a [Caption] -> IO T.Text
convertToHTML video dir parsed = case parsed of
  Right captions -> do
    html <- captionsToHTML video dir captions
    hPutStrLn stderr "Finished"
    return html
  Left err -> return (T.pack (show err))

captionsToHTML :: Video -> Dir -> [Caption] -> IO T.Text
captionsToHTML video dir captions = do
  caps <- formatCaptions captions (coerce $ url video) dir
  deleteImages dir
  let embodied = embody video caps
  return (heading <> embodied)

formatCaptions :: [Caption] -> Url -> Dir -> IO T.Text
formatCaptions captions (Url url) (Dir dir) = do
  let toMap = fromList $ captionsPerSlide captions
  let listy = toList (mapWithIndex (imgCaps url dir) toMap)
  intercalate "\n" <$> sequence listy

imgCaps url dir ind captions = do
  img <- slideBlock url dir (toInteger ind + 1)
  return (img <> caps captions <> "</div>")

slideBlock :: T.Text -> T.Text -> Integer -> IO T.Text
slideBlock url dir shot = do
  let imgPath = T.unpack (dir <> "/glancer-img" <> T.pack (printf "%04d.jpg" shot))
  (_, jpg, _) <- B.readProcessWithExitCode "/bin/cat" [imgPath] ""
  (_, base64, _) <- B.readProcessWithExitCode "base64" [] jpg
  let when = T.pack $ show (shotSeconds shot 30)
  let slide = "<div id='slide" <> T.pack (show shot) <> "'class='slide-block'>\n"
  let diva = "\t<div class='img'><a href='" <> url <> "&t=" <> when <> "'>\n"
  let img = "\t\t<img src='data:image/jpeg;base64, " <> decodeUtf8 base64 <> "'/></a>\n"
  let close = "\t</div>\n"
  let formatted = slide <> diva <> img <> close
  return formatted

caps :: [Caption] -> T.Text
caps captions = "\t<div class='txt'>\n" <> intercalate "\n" (map (<> "</br>") (stripped captions)) <> "\n\t</div>"
  where
    stripped captions = map (("\t\t" <>) . strip . txt) captions

captionsPerSlide :: [Caption] -> [[Caption]]
captionsPerSlide captions = map (capsForShot (filtering captions) 30) (shots (filtering captions))
  where
    filtering lst = filter (not . (T.isInfixOf "<c>" . txt)) lst
    shots captions = [1 .. numShots (filtering captions) 30]

shotSeconds :: Integer -> Integer -> Integer
shotSeconds shotNumber secsPerShot = shotNumber * secsPerShot

hourMinuteSeconds :: Integer -> TimeDef
hourMinuteSeconds seconds = TimeDef h m s 0
  where
    h = seconds `div` 3600
    m = seconds `mod` 3600 `div` 60
    s = seconds `mod` 3600 `mod` 60

shotTime :: Integer -> Integer -> TimeBlock
shotTime shotNumber secsPerShot = TimeBlock startTime endTime
  where
    startTime = hourMinuteSeconds start_
    endTime = hourMinuteSeconds end_
    start_ = shotSeconds (shotNumber - 1) secsPerShot
    end_ = shotSeconds shotNumber secsPerShot

numShots :: [Caption] -> Integer -> Integer
numShots caps secsPerShot = secs (end . block . last $ caps) `div` secsPerShot

capsForShot :: [Caption] -> Integer -> Integer -> [Caption]
capsForShot caps secsPerShot shotNumber = filter (inTimeBlock shotBlock) caps
  where
    shotBlock = shotTime shotNumber secsPerShot
    inTimeBlock blck cap = inBlock (start . block $ cap) blck && inBlock (end . block $ cap) blck
