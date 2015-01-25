module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    addScript $ StaticR ghcjs_test_jsexe_all_js
    setTitle "ghcjs-jquery tests"
    $(widgetFile "homepage")

postTest1R :: Handler TypedContent
postTest1R = do
  selectRep $ do
    provideRep $ return ("all ok" :: Text)

