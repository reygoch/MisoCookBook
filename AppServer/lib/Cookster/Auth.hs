{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
--
module Cookster.Auth where
--
import           Servant.Auth.Server      ( ToJWT, FromJWT )

import           Data.Text                ( Text )
import           Data.Aeson               ( ToJSON, FromJSON )

import qualified GHC.Generics as GHC      ( Generic )
import qualified Generics.SOP as SOP      ( Generic, HasDatatypeInfo )

import           Cookster.DataLayer.Model ( ID, User (..) )
--

data Token = Token
  { id       :: ID User
  , username :: Text
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON, ToJWT, FromJWT )

user2token :: User -> Token
user2token ( User uID uName _ ) = Token uID uName
