{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Html where

import qualified Data.Text as T
import Process (Title (..), Url (..), Video (..))
import Text.RawString.QQ (r)

heading :: T.Text
heading =
  T.pack
    [r|
<!doctype html>
<html>

<head>
    <style>
        h1 {
            text-align: center;
        }

        a {
            color: darkgreen;
            text-decoration: none;
        }

        a:hover {
            text-decoration: underline;
        }

        #container {
            padding: 3%;
            margin: auto;
            width: 96%;
        }

        .slide-block {
            margin-top:1%;
            border: 1px solid black;
            border-radius: 3px;
            display: block;
            width: 100%;
        }

        .img {
            padding: 1%;
            width: 65%;
            display: inline-block;
        }

        .img:hover{
            transform:scale(1.3) translate(15%);
        }

        img {
            max-width: 100%;
            max-height: 100%;
        }

        .txt {
            margin-left: 3%;
            line-height: 1.5;
            width: 29%;
            display: inline-block;
            font-size: 22px;
            vertical-align: top;
            margin-top: 1%;
        }
    </style>
</head>
|]

embody :: Video -> T.Text -> T.Text
embody (Video (Url url) (Title title) _) body =
  T.intercalate
    "\n"
    [ "\t<body>",
      "\t\t<div id='container'>",
      "\t\t\t<h1><a href='" <> url <> "'>" <> title <> "</a></h1>",
      body,
      "</body>",
      "</html>"
    ]
