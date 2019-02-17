{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
--
module Cookster.AppServer where
--
import Prelude                     hiding ( readFile, writeFile )

import Servant                     hiding ( Server )
import Servant.Auth.Server                ( CookieSettings (..), JWTSettings (..), IsSecure (..) , defaultCookieSettings, defaultJWTSettings, generateKey )
import Servant.Server.Generic             ( genericServerT )

import Network.Wai.Handler.Warp           ( run )

import Data.Text                          ( Text, pack, unpack )
import Data.Aeson                         ( decode', encode )
import Data.ByteString.Lazy               ( readFile, writeFile )

import System.IO.Error                    ( tryIOError )
import Control.Exception                  ( bracket )

import Control.Monad.IO.Class             ( MonadIO (..) )
import Control.Monad.Trans.Maybe          ( MaybeT (..), maybeToExceptT )
import Control.Monad.Trans.Except         ( ExceptT (..), runExceptT, withExceptT )

import Crypto.JOSE.JWK                    ( JWK )

import Cookster.Api                       ( appApi, appServer )
import Cookster.App                       ( Context (..), conAppM )
import Cookster.Settings                  ( Settings (..), Server (..), readSettings )
import Cookster.DataLayer.Database        ( createPool, deletePool )
import Cookster.DataLayer.Model.Password  ( makeHashingPolicy )
--

data InitError = JWKError | SettingsError Text

start :: IO ()
start = do
  starter <- runExceptT $ do
    settings <- withExceptT ( SettingsError . pack . show ) $
                ExceptT $ readSettings "dat/settings.yaml"
    tokenkey <- maybeToExceptT JWKError $ MaybeT $
                loadJWK $ jkey $ server $ settings

    let jss = defaultJWTSettings tokenkey
        css = defaultCookieSettings
              { cookieIsSecure    = NotSecure
              , cookieXsrfSetting = Nothing
              }
        ctx = css :. jss :. EmptyContext
        pxy = Proxy :: Proxy '[ CookieSettings, JWTSettings ]
        api = appApi
        hsp = makeHashingPolicy $ password settings
        prt = fromIntegral $ port $ server settings

    let act = Context jss css

    pure $ bracket ( createPool $ database settings ) deletePool
         $ \ p -> run prt
         $ serveWithContext api ctx
         $ hoistServerWithContext api pxy ( conAppM act p )
         $ genericServerT
         $ appServer

  case starter of
    Left JWKError              -> putStrLn "unable to create JWK"
    Left ( SettingsError msg ) -> putStrLn $ unpack msg
    Right server               -> server -- start the server !

  where loadJWK :: MonadIO m => FilePath -> m ( Maybe JWK )
        loadJWK fp = liftIO $ do
          file <- tryIOError $ readFile fp
          case file of
            Right kf -> pure $ decode' kf
            Left  _  -> do
              key <- generateKey
              writeFile fp $ encode key
              pure $ Just key
