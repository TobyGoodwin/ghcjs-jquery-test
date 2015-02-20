module Handler.Home where

import Import

import Data.Aeson (decode)
import qualified Data.Map as Map
import Network.Wai (strictRequestBody)

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    addScript $ StaticR ghcjs_test_jsexe_all_js
    setTitle "ghcjs-jquery tests"
    $(widgetFile "homepage")

postJsonR :: Handler TypedContent
postJsonR = do
  r <- getRequest
  b <- liftIO $ strictRequestBody $ reqWaiRequest r
  liftIO $ print b
  let c = decode b :: Maybe (Map String String)
  liftIO $ print c
  selectRep $ provideRep $ return ("json ok" :: Text)

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
