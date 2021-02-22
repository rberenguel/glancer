{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Text (pack)
import qualified Parser as P
import Test.Hspec ( hspec, describe, it, Spec )
import Test.Hspec.Megaparsec ( shouldParse )
import Text.Megaparsec ( parse )
import Text.RawString.QQ ( r )

main :: IO ()
main = hspec spec

hourBlock = "01:07:46.029 "

parsedHourBlock = P.TimeDef 1 7 46 29

timeBlock =
  pack
    [r|01:07:46.029 --> 01:07:52.319 align:start position:0%
|]

parsedTimeBlock = P.TimeBlock parsedHourBlock (P.TimeDef 1 7 52 319)

captionBlock =
  pack
    [r|
01:07:46.029 --> 01:07:52.319 align:start position:0%
really look into but we have considered
|]

parsedCaptionBlock = P.Caption parsedTimeBlock "really look into but we have considered\n"

spec :: Spec
spec = do
  describe "arrowP" $ do
    it "parses a time arrow" $
      parse P.arrowP "" "-->" `shouldParse` "-->"
  describe "hourBlockP" $ do
    it "parses a hourBlock" $
      parse P.hourBlockP "" hourBlock `shouldParse` parsedHourBlock
  describe "timeBlockP" $ do
    it "parses a timeBlock" $
      parse P.timeLineP "" timeBlock `shouldParse` parsedTimeBlock
  describe "captionP" $ do
    it "parses a captionBlock" $ parse P.captionP "" captionBlock `shouldParse` parsedCaptionBlock
