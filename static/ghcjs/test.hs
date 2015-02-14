{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude

import JavaScript.JQuery

main = do
  t "nonexistent" "nonesuch" string 404 Nothing
  t "string" "plain" string 200 (Just "plain ok")
  t "text" "plain" text 200 (Just "plain ok")
  t "pairsS" "pairs" pairsS 200 (Just "pairs ok")
  t "pairsT" "pairs" pairsT 200 (Just "pairs ok")
  where
    t name url input rStatus rData = do
      say $ "about to run test " ++ name
      r <- ajax3 url input
      if arStatus r == rStatus && arData r == rData
        then say $ name ++ " passed"
        else say $ name ++ " failed: r is " ++ tshow r

string = "some string" :: String
text = "some text" :: Text
pairsS =
  [("one","pair"),("another","couple"),("and","third")] :: [(String,String)]
pairsT =
  [("one","pair"),("another","couple"),("and","third")] :: [(Text,Text)]

say :: Text -> IO ()
say x = do
  y <- select $ "<p>" ++ x ++ "</p>"
  void $ select "body" >>= appendJQuery y
