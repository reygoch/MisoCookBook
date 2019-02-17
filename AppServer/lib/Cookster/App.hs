{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}
--
module Cookster.App where
--
import Data.Aeson                                   ( ToJSON )
import Data.ByteString                              ( ByteString )
import Data.ByteString.Lazy                         ( toStrict )

import Servant                                      ( Handler, throwError, err500 )
import Servant.Auth.Server                          ( ToJWT, CookieSettings, JWTSettings, makeJWT )
import Servant.Server.Generic                       ( AsServerT )

import Crypto.JOSE.JWK                              ( JWK )

import GHC.Generics                                 ( Generic )

import Control.Monad                                ( (>=>) )
import Control.Monad.IO.Class                       ( liftIO )
import Control.Monad.Freer                          ( Members, Eff, runM, send )
import Control.Monad.Freer.Error                    ( Error, runError )
import Control.Monad.Freer.Reader                   ( Reader, runReader, asks )

import Cookster.DataLayer.Schema                    ( Schema )
import Cookster.DataLayer.Database                  ( DBPool, DB, runDB )
import Cookster.DataLayer.Repository.UserRepo       ( UserRepo, runUserRepo )
import Cookster.DataLayer.Repository.IngredientRepo ( IngredientRepo, runIngredientRepo )
--

data AppE
  = SomeThingWong
  | UnableToMakeJWT
  | BadLogin
  deriving ( Eq, Show, Generic, ToJSON )

type AppM = Eff '[ UserRepo, IngredientRepo, Error AppE, DB, Reader Context, IO ]

type AsAppM = AsServerT AppM

data Context = Context
  { jss :: JWTSettings
  , css :: CookieSettings
  }

hdlAppE :: AppE   -> Handler a
hdlAppE SomeThingWong = throwError err500

conAppM :: Context -> DBPool Schema -> AppM a -> Handler a
conAppM c p = liftIO . ( runAppM c p ) >=> either hdlAppE pure

runAppM :: Context -> DBPool Schema -> AppM a -> IO ( Either AppE a )
runAppM c p = runM . runReader c . runDB p . runError . runIngredientRepo . runUserRepo

makeJWT' :: Members '[ Reader Context, IO ] f => ToJWT a => a -> Eff f ( Either AppE ByteString )
makeJWT' x = do
  s <- asks jss
  t <- send $ makeJWT x s Nothing
  pure $ case t of
    Left _  -> Left UnableToMakeJWT
    Right t -> Right $ toStrict t
  -- pure $ either ( const $ Left UnableToMakeJWT ) ( Right ) $ toStrict t
