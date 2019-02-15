{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
--
module Cookster.DataLayer.Model where
--
import           Data.Text                         ( Text )
import qualified GHC.Generics               as GHC ( Generic )
import qualified Generics.SOP               as SOP ( Generic, HasDatatypeInfo )
import           Squeal.PostgreSQL
import           Cookster.DataLayer.Model.Password ( Hash (..), Password )
--

data Unit = L | Kg | Pce
  deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

type PGUnit = 'PGenum '[ "L", "Kg", "Pce" ]

type instance PG Unit = PGUnit

instance PGTyped   schema ( nullity PGUnit ) where pgtype    = UnsafeTypeExpression "_Unit"
instance ToParam   Unit   PGUnit             where toParam   = toParam . Enumerated
instance FromValue PGUnit Unit               where fromValue = getEnumerated <$> fromValue @PGUnit

--

newtype Image = Image
  { unImage :: Text
  } deriving ( Eq, Show, GHC.Generic )

data Credentials ( h :: Hash ) = Credentials
  { username :: Text
  , password :: Password h
  } deriving ( Eq, Show, GHC.Generic )

--

newtype ID a = ID
  { unID :: a
  } deriving ( Eq, Show, GHC.Generic )

data User = User
  { id       :: ID User
  , username :: Text
  , password :: Password Hashed
  } deriving ( Eq, Show, GHC.Generic )

data Ingredient = Ingredient
  { id          :: ID Ingredient
  , name        :: Text
  , image       :: Image
  , description :: Text
  , unit        :: Unit
  , cost        :: Word
  } deriving ( Eq, Show, GHC.Generic )

data Recipe = Recipe
  { id          :: ID Recipe
  , name        :: Text
  , image       :: Maybe Image
  , description :: Maybe Text
  } deriving ( Eq, Show, GHC.Generic )

data RecipeIngredient = RecipeIngredient
  { id           :: ID RecipeIngredient
  , recipeId     :: ID Recipe
  , ingredientId :: ID Ingredient
  , amount       :: Double
  } deriving ( Eq, Show, GHC.Generic )

data RecipeInstruction = RecipeInstruction
  { id          :: ID RecipeInstruction
  , recipeId    :: ID Recipe
  , ordering    :: Word
  , instruction :: Text
  } deriving ( Eq, Show, GHC.Generic )

data UserRecipe = UserRecipe
  { id       :: ID UserRecipe
  , userId   :: ID User
  , recipeId :: ID Recipe
  } deriving ( Eq, Show, GHC.Generic )
