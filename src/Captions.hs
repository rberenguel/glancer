{-# LANGUAGE OverloadedStrings #-}

module Captions where

import Data.Coerce (coerce)
import Data.Foldable (Foldable (toList))
import Data.Sequence (fromList, mapWithIndex)
import Data.Text (intercalate, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Html (embody, heading)
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
  let listy = toList (mapWithIndex (imgCaps url (T.pack dir)) toMap)
  intercalate "\n" <$> sequence listy

imgCaps :: Integral a => T.Text -> T.Text -> a -> [Caption] -> IO T.Text
imgCaps url dir ind captions = do
  --let next = toInteger ind + 1
  img <- slideBlock url dir (toInteger ind) --next
  let toVideo = toVideoBlock url (toInteger ind)
  return (img <> caps captions <> toVideo <> "</div>")

slideBlock :: T.Text -> T.Text -> Integer -> IO T.Text
slideBlock url dir shot = do
  let imgPath = T.unpack (dir <> "/glancer-img" <> T.pack (printf "%04d.jpg" shot))
  (_, jpg, _) <- B.readProcessWithExitCode "/bin/cat" [imgPath] ""
  (_, base64, _) <- B.readProcessWithExitCode "base64" [] jpg
  let slide = "<div id='slide" <> T.pack (show shot) <> "'class='slide-block'>\n"
  let div = "\t<div class='img'>\n"
  let img = "\t\t<img src='data:image/jpeg;base64, " <> decodeUtf8 base64 <> "'/></a>\n"
  let close = "\t</div>\n"
  let formatted = slide <> div <> img <> close
  return formatted

toVideoBlock :: T.Text -> Integer -> T.Text
toVideoBlock url shot = do
  let when = T.pack $ show (shotSeconds shot 30)
  let title = "title='Go to video at timestamp " <> when <> "s'"
  let diva = "<div class='to-video'><a " <> title <> "href='" <> url <> "&t=" <> when <> "s'>&#8688;</a></div>"
  diva

caps :: [Caption] -> T.Text
caps captions = "\t<div class='txt'>\n" <> intercalate "\n" (stripped captions) <> "\n\t</div>"
  where
    stripped captions = map (("\t\t" <>) . strip . txt) captions

captionsPerSlide :: [Caption] -> [[Caption]]
captionsPerSlide captions = map (capsForShot (filtering captions) 30) (shots (filtering captions))
  where
    filtering lst = filter (not . (T.isInfixOf "<c>" . txt)) lst
    shots captions = [1 .. numShots captions 30 + 1]

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
