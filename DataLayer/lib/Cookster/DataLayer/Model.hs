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
import           Data.Word                         ( Word32, Word64 )
import qualified GHC.Generics               as GHC ( Generic )
import qualified Generics.SOP               as SOP ( Generic, HasDatatypeInfo )
import           Generics.SOP.BasicFunctors        ( K (..) )
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
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

type instance PG Image = PGtext

--

data Credentials ( h :: Hash ) = Credentials
  { username :: Text
  , password :: Password h
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

--

data Pagination = Pagination
  { items :: Word64 -- ^ items per page
  , pagen :: Word64 -- ^ page number
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

--

newtype ID a = ID
  { unID :: Word32
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

type instance PG ( ID a ) = PGint4

--

data Entity e = Entity
  { entityId :: ID e
  , entity   :: e
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

--

data User = User
  { id       :: ID User
  , username :: Text
  , password :: Password Hashed
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

data User' = User'
  { username :: Text
  , password :: Password Hashed
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

--

data Ingredient = Ingredient
  { id          :: ID Ingredient
  , name        :: Text
  , image       :: Image
  , description :: Text
  , cost        :: Word32
  , unit        :: Unit
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

data NewIngredient = NewIngredient
  { name        :: Text
  , image       :: Image
  , description :: Text
  , cost        :: Word32
  , unit        :: Unit
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

--

data Recipe = Recipe
  { id          :: ID Recipe
  , userId      :: ID User
  , name        :: Text
  , image       :: Maybe Image
  , description :: Maybe Text
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

data NewRecipe = NewRecipe
  { userId      :: ID User
  , name        :: Text
  , image       :: Maybe Image
  , description :: Maybe Text
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

--

data RecipeIngredient = RecipeIngredient
  { id           :: ID RecipeIngredient
  , recipeId     :: ID Recipe
  , ingredientId :: ID Ingredient
  , amount       :: Double
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

data NewRecipeIngredient = NewRecipeIngredient
  { recipeId     :: ID Recipe
  , ingredientId :: ID Ingredient
  , amount       :: Double
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

--

data RecipeInstruction = RecipeInstruction
  { id          :: ID RecipeInstruction
  , recipeId    :: ID Recipe
  , ordering    :: Word32
  , instruction :: Text
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

data NewRecipeInstruction = NewRecipeInstruction
  { recipeId    :: ID Recipe
  , ordering    :: Word32
  , instruction :: Text
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

--

data UserRecipe = UserRecipe
  { id       :: ID UserRecipe
  , userId   :: ID User
  , recipeId :: ID Recipe
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )

data NewUserRecipe = NewUserRecipe
  { userId   :: ID User
  , recipeId :: ID Recipe
  } deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo )
