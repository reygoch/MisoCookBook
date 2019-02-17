{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Cookster.Api where
--
import Data.ByteString          ( ByteString )
import Data.Text.Encoding       ( decodeUtf8 )
import Crypto.JWT               ( ClaimsSet )
import Servant                  ( Server, ReqBody, Get, JSON, PlainText, (:>), (:<|>) (..) )
import Servant.API.Generic      ( ToServantApi, (:-) )
import Servant.Auth.Server      ( Cookie, JWT, Auth, AuthResult (..), encodeJWT )

import Data.Text                ( Text )
import Data.Proxy               ( Proxy (..) )

import GHC.Generics             ( Generic )

import Control.Monad.Freer.Error( throwError )

import Cookster.App             ( AsAppM, AppE (..), makeJWT' )
import Cookster.Auth            ( Token, user2token )

import Cookster.DataLayer.Model ( ID (..), Image (..), Cost (..), Unit (..), Ingredient (..), Credentials (..) )
import Cookster.DataLayer.Model.Password ( Hash (Plain) )
import Cookster.DataLayer.Repository.IngredientRepo ( selectAllIngredients' )
import Cookster.DataLayer.Repository.UserRepo ( createUser', selectUserByUsername' )
--

type AuthMethods = '[ Cookie, JWT ]

data API ( auth :: [*] ) route = API
  { root :: route :-          Get '[ PlainText ] Text
  , subr :: route :- "sub" :> Get '[ PlainText ] Text
  , ingr :: route :- "ing" :> Get '[ JSON ] [ Ingredient ]
  , auth :: route :- "auth" :> ReqBody '[ JSON ] ( Credentials 'Plain ) :> Get '[ PlainText ] Text
  , angr :: route :- "ing" :> "auth" :> Auth auth Token :> Get '[ JSON ] [ Ingredient ]
  } deriving Generic

appApi :: Proxy ( ToServantApi ( API AuthMethods ) )
appApi = Proxy

appServer :: API auths AsAppM
appServer = API
  { root = pure "Root"
  , subr = pure "Sub Route"
  , ingr = selectAllIngredients'
  , auth = \ c -> do
    mu <- selectUserByUsername' ( username c )
    case mu of
      Nothing -> throwError BadLogin
      Just u  -> do
        et <- makeJWT' $ user2token u
        case et of
          Left  e -> throwError e
          Right t -> pure $ decodeUtf8 t
  , angr = \ t -> case t of
    Authenticated _ -> selectAllIngredients'
    _               -> throwError SomeThingWong
  }
