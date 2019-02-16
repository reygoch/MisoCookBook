{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Cookster.Api where
--
import Servant                  ( Server, Get, JSON, PlainText, (:>), (:<|>) (..) )
import Servant.API.Generic      ( ToServantApi, (:-) )
import Servant.Auth.Server      ( Cookie, JWT, Auth )
import Data.Text                ( Text )
import Data.Proxy               ( Proxy (..) )
import GHC.Generics             ( Generic )
import Cookster.App             ( AsAppM )
import Cookster.DataLayer.Model ( ID (..), Image (..), Cost (..), Unit (..), Ingredient (..) )
import Cookster.DataLayer.Repository.IngredientRepo ( selectAllIngredients' )
--

type AuthMethods = '[ Cookie, JWT ]

data API ( auth :: [*] ) route = API
  { root :: route :-          Get '[ PlainText ] Text
  , subr :: route :- "sub" :> Get '[ PlainText ] Text
  , ingr :: route :- "ing" :> Get '[ JSON ]      [ Ingredient ]
  } deriving Generic

appApi :: Proxy ( ToServantApi ( API AuthMethods ) )
appApi = Proxy

appServer :: API auths AsAppM
appServer = API
  { root = pure "Root"
  , subr = pure "Sub Route"
  , ingr = selectAllIngredients'
  }
