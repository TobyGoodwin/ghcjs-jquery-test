{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ClassyPrelude

import Data.Aeson (ToJSON, toJSON)
import JavaScript.JQuery

data Test o = Test { name :: Text
                   , url :: Text
                   , input :: InType
                   , rStatus :: Int
                   , rData :: o
                   }

data InType = S String | T Text | P [(Text,Text)]
instance ToAjax InType
  where toAjax (S x) = toAjax x
        toAjax (T x) = toAjax x
        toAjax (P x) = toAjax x

instance ToJSON InType
  where toJSON (S x) = toJSON x
        toJSON (T x) = toJSON x
        toJSON (P x) = toJSON x

tests =
  [ Test "nonexistent" "nonesuch" (S "some string") 404 Nothing
  , Test "string" "plain" (S "some string") 200 (Just "plain ok")
  , Test "text" "plain" (T "some text") 200 (Just "plain ok")
  , Test "pairs" "pairs"
      (P [("one","pair"),("another","couple")]) 200 (Just "pairs ok")
  ]

main = mapM_ runTest tests
  where
    runTest t = do
      say $ "about to run test " ++ name t
      r <- ajax3 (url t) (input t)
      if arStatus r == rStatus t && arData r == rData t
        then say $ name t ++ " passed"
        else say $ name t ++ " failed: r is " ++ tshow r

say :: Text -> IO ()
say x = do
  y <- select $ "<p>" ++ x ++ "</p>"
  void $ select "body" >>= appendJQuery y
