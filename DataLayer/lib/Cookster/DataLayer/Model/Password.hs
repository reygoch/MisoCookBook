{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE KindSignatures         #-}
--
module Cookster.DataLayer.Model.Password where
--
import           Data.Text                         ( Text )
import qualified GHC.Generics               as GHC ( Generic )
--

data Hash = Plain | Hashed
  deriving ( Eq, Show, GHC.Generic )

newtype Password ( h :: Hash ) = Password
  { unPassword :: Text
  } deriving ( Eq, Show, GHC.Generic )
