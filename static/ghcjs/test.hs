{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude hiding (fromList)

import Data.Aeson
import Data.Default
import Data.String.Conversions
import JavaScript.JQuery

-- test of js conversion

import Data.HashMap.Strict
import qualified Data.Text as T
import GHCJS.Marshal
import GHCJS.Types

foreign import javascript unsafe 
  "console.log($1)"
  clog :: JSRef Value ->IO ()

test0 = Object (fromList [("type",String "POST"),("contentType",String "text/json; charset=UTF-8"),("data",Object (fromList [("key",String "value"),("other",String "another")])),("processData",Bool False)])

main = do
  let (++) = T.append
  test1 <- toJSRef test0
  test2 <- fromJSRef test1 :: IO (Maybe Value)
  putStrLn $ "test2 is " ++ tshow test2
  clog test1
  t "nonexistent" "nonesuch" string 404 Null
  t "getText" "getText" () 200 (String "the cat sat on the mat")
  t "getJson" "getJson" () 200 json1
  t "string" "plain" string 200 (String "plain ok")
  t "text" "plain" text 200 (String "plain ok")
  -- only support [(Text,Text)] so far
  -- t "pairsS" "pairs" pairsS 200 (Just "pairs ok")
  t "pairsT" "pairs" pairsT 200 (String "pairs ok")
  t "personOk" "personOk" json0 200 (String "json ok")
  t "personEcho" "personEcho" json0 200 json0
  where
    t name url input rStatus rData = do
      r <- ajax url input def
      print r
      if arStatus r == rStatus && arData r == rData
        then say $ name ++ " passed"
        else say $ name ++ " failed: r is " ++ tshow r

string = "some string" :: String
text = "some text" :: Text
pairsS =
  [("one","pair"),("another","couple"),("and","third")] :: [(String,String)]
pairsT =
  [("one","pair"),("another","couple"),("and","third")] :: [(Text,Text)]

jPerson :: Text -> Int -> Value
jPerson n h = object [ ("name" :: Text) .= n
                     , ("height" :: Text) .= h
                     ]
json0 = jPerson "Toby" 195
json1 = jPerson "Gary Getter" 173


say :: Text -> IO ()
say x = do
  y <- select $ "<p>" ++ x ++ "</p>"
  void $ select "body" >>= appendJQuery y
