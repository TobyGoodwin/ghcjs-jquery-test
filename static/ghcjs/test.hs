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
  t "nonexistent" "nonesuch" string 404 Nothing
  t "string" "plain" string 200 (Just "plain ok")
  t "text" "plain" text 200 (Just "plain ok")
  -- only support [(Text,Text)] so far
  -- t "pairsS" "pairs" pairsS 200 (Just "pairs ok")
  t "pairsT" "pairs" pairsT 200 (Just "pairs ok")
  t "personOk" "personOk" json0 200 (Just "json ok")
  t "personEcho" "personEcho" json0 200 (Just $ cs $ encode json0)
  where
    t name url input rStatus rData = do
      r <- ajax url input def
      if arStatus r == rStatus && arData r == rData
        then say $ name ++ " passed"
        else say $ name ++ " failed: r is " ++ tshow r

string = "some string" :: String
text = "some text" :: Text
pairsS =
  [("one","pair"),("another","couple"),("and","third")] :: [(String,String)]
pairsT =
  [("one","pair"),("another","couple"),("and","third")] :: [(Text,Text)]
json0 = object [ ("name" :: Text) .= ("Toby" :: Text)
               , ("height" :: Text) .= (195 :: Int)
               ]


say :: Text -> IO ()
say x = do
  y <- select $ "<p>" ++ x ++ "</p>"
  void $ select "body" >>= appendJQuery y
