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

        h1 > a:hover {
            text-decoration: underline;
        }
        #container {
            padding: 3%;
            margin: auto;
            width: 96%;
        }

        .to-video {
            font-size: 30px;
            float: right;
            align-self: flex-end;
            display: inline-block;
            vertical-align: bottom;
        }
    
        .slide-block {
            margin-top:1%;
            border: 1px solid black;
            border-radius: 3px;
            display: flex;
            width: 100%;
        }

        .img {
            transform:scale(1.0) translate(0%);
            transition: transform 1.2s ease, border 0.1s ease; 
            padding: 1%;
            width: 65%;
            display: inline-block;
        }

        .img:hover{
            transform:scale(1.4) translate(13%);
			border: 1px solid black;
			background: rgba(255, 255, 255, 0.9);
			-webkit-backdrop-filter: blur(10px);
  			backdrop-filter: blur(10px);
			z-index: 1000;
			border-radius: 3px;
        }

        img {
            max-width: 100%;
            max-height: 100%;
        }

        .txt {
            margin-left: 1%;
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
