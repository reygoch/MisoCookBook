{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
--
module Cookster.DataLayer.Model.Password where
--
import           Data.Text                              ( Text )
import qualified GHC.Generics               as GHC      ( Generic )
import qualified Generics.SOP               as SOP      ( Generic, HasDatatypeInfo, K )
import           Generics.SOP.BasicFunctors             ( K (..) )
import           Squeal.PostgreSQL                      ( PG, PGType ( PGtext ) )
--

data Hash = Plain | Hashed
  deriving ( Eq, Show, GHC.Generic )

newtype Password ( h :: Hash ) = Password
  { unPassword :: Text
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

type instance PG ( Password 'Hashed ) = PGtext
