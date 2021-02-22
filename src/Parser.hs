{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead, try),
    Parsec,
    manyTill,
    (<|>),
  )
import Text.Megaparsec.Char
  ( asciiChar,
    char,
    eol,
    letterChar,
    numberChar,
    space,
    spaceChar,
    string,
  )

type Parser = Parsec Void T.Text

data TimeDef = TimeDef
  { hours :: Integer,
    minutes :: Integer,
    seconds :: Integer,
    cents :: Integer
  }
  deriving (Show, Eq)

secs :: TimeDef -> Integer
secs td = 3600 * hours td + 60 * minutes td + seconds td + 1

instance Ord TimeDef where
  compare a b = compare (secs a) (secs b)

data TimeBlock = TimeBlock
  { start :: TimeDef,
    end :: TimeDef
  }
  deriving (Show, Eq)

inBlock :: TimeDef -> TimeBlock -> Bool
inBlock td tb = (secs . start $ tb) <= secstd && secstd <= (secs . end $ tb)
  where
    secstd = secs td

data Caption = Caption
  { block :: TimeBlock,
    txt :: T.Text
  }
  deriving (Show, Eq)

hourBlockP :: Parser TimeDef
hourBlockP = do
  hours <- colonSeparated
  minutes <- colonSeparated
  seconds <- read <$> manyTill numberChar (char '.')
  millis <- read <$> manyTill numberChar (void spaceChar <|> try (lookAhead (void eol)))
  return (TimeDef hours minutes seconds millis)
  where
    colonSeparated = read <$> manyTill numberChar (char ':')

arrowP :: Parser T.Text
arrowP = do
  string "-->"

anyChar :: Parser Char
anyChar = letterChar <|> spaceChar <|> asciiChar

timeLineP :: Parser TimeBlock
timeLineP = do
  start <- hourBlockP
  arrowP
  space
  end <- hourBlockP
  manyTill anyChar eol
  return (TimeBlock start end)

captionP :: Parser Caption
captionP = do
  space
  block <- timeLineP
  caption <- T.pack <$> manyTill anyChar (try $ lookAhead (void timeLineP <|> void eof))
  return (Caption block caption)

subsP :: Parser [Caption]
subsP = do
  string "WEBVTT"
  manyTill anyChar (try $ lookAhead timeLineP)
  space
  manyTill captionP eof
