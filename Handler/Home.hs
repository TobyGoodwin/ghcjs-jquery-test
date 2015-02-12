module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    addScript $ StaticR ghcjs_test_jsexe_all_js
    setTitle "ghcjs-jquery tests"
    $(widgetFile "homepage")

postPlainR :: Handler TypedContent
postPlainR = selectRep $ provideRep $ return ("plain ok" :: Text)
