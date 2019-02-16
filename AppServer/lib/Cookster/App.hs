{-# LANGUAGE DataKinds #-}
--
module Cookster.App where
--
import Servant                                      ( Handler, throwError, err500 )
import Servant.Server.Generic                       ( AsServerT )

import Control.Monad                                ( (>=>) )
import Control.Monad.IO.Class                       ( liftIO )
import Control.Monad.Freer                          ( Eff, runM )
import Control.Monad.Freer.Error                    ( Error, runError )

import Cookster.DataLayer.Schema                    ( Schema )
import Cookster.DataLayer.Database                  ( DBPool )
import Cookster.DataLayer.Repository.IngredientRepo ( IngredientRepo, runIngredientRepo )
--

data AppE = SomeThingWong

type AppM = Eff '[ IngredientRepo, Error AppE, IO ]

type AsAppM = AsServerT AppM

hdlAppE :: AppE   -> Handler a
hdlAppE SomeThingWong = throwError err500

conAppM :: DBPool Schema -> AppM a -> Handler a
conAppM p = liftIO . ( runAppM p ) >=> either hdlAppE pure

runAppM :: DBPool Schema -> AppM a -> IO ( Either AppE a )
runAppM p = runM . runError . runIngredientRepo p
