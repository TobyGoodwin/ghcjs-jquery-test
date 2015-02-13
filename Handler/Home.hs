module Handler.Home where

import Import

import qualified Data.Map as Map

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    addScript $ StaticR ghcjs_test_jsexe_all_js
    setTitle "ghcjs-jquery tests"
    $(widgetFile "homepage")

postPlainR :: Handler TypedContent
postPlainR = selectRep $ provideRep $ return ("plain ok" :: Text)

postPairsR :: Handler TypedContent
postPairsR = do
  (e, f) <- postEnv
  print e
  selectRep $ provideRep $ return ("pairs ok" :: Text)

-- from Yesod.Form.Functions
postEnv :: (MonadHandler m, MonadResource m)
        => m (Env, FileEnv)
postEnv = do
    (p, f) <- runRequestBody
    let p' = Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) p
    return $ (p', Map.unionsWith (++) $ map (\(k, v) -> Map.singleton k [v]) f)
