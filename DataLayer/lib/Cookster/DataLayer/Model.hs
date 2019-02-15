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
import           Data.Int                          ( Int32 )
import           Data.Word                         ( Word32, Word64 )
import           Data.Aeson                        ( ToJSON, FromJSON )
import qualified GHC.Generics               as GHC ( Generic )
import qualified Generics.SOP               as SOP ( Generic, HasDatatypeInfo )
import           Generics.SOP.BasicFunctors        ( K (..) )
import           Squeal.PostgreSQL
import           Squeal.PostgreSQL.Binary          ( FromValue (..) )
import           Cookster.DataLayer.Model.Password ( Hash (..), Password )
--

data Unit = L | Kg | Pce
  deriving ( Eq, Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo, ToJSON, FromJSON )

type PGUnit = 'PGenum '[ "L", "Kg", "Pce" ]

type instance PG Unit = PGUnit

instance PGTyped   schema ( nullity PGUnit ) where pgtype    = UnsafeTypeExpression "_Unit"
instance ToParam   Unit   PGUnit             where toParam   = toParam . Enumerated
instance FromValue PGUnit Unit               where fromValue = getEnumerated <$> fromValue @PGUnit

--

newtype Image = Image
  { unImage :: Text
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

type instance PG Image = PGtext

instance FromValue PGtext Image where
  fromValue = Image <$> fromValue @'PGtext

--

data Credentials ( h :: Hash ) = Credentials
  { username :: Text
  , password :: Password h
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

--

data Pagination = Pagination
  { items :: Word64 -- ^ items per page
  , pagen :: Word64 -- ^ page number
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

--

newtype Cost = Cost
  { unCost :: Word32
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

type instance PG Cost = PGint4

instance FromValue PGint4 Cost where
  fromValue = Cost . ( fromIntegral @Int32 ) <$> fromValue @'PGint4

--

newtype ID a = ID
  { unID :: Word32
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

type instance PG ( ID a ) = PGint4

instance FromValue PGint4 ( ID a ) where
  fromValue = ID . ( fromIntegral @Int32 ) <$> fromValue @'PGint4

--

data Entity e = Entity
  { entityId :: ID e
  , entity   :: e
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

--

data User = User
  { id       :: ID User
  , username :: Text
  , password :: Password Hashed
  } deriving
    ( Eq, Show, GHC.Generic
    , SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

data User' = User'
  { username :: Text
  , password :: Password Hashed
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

--

data Ingredient = Ingredient
  { id          :: ID Ingredient
  , name        :: Text
  , image       :: Image
  , description :: Text
  , cost        :: Cost
  , unit        :: Unit
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

data NewIngredient = NewIngredient
  { name        :: Text
  , image       :: Image
  , description :: Text
  , cost        :: Word32
  , unit        :: Unit
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

--

data Recipe = Recipe
  { id          :: ID Recipe
  , userId      :: ID User
  , name        :: Text
  , image       :: Maybe Image
  , description :: Maybe Text
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

data NewRecipe = NewRecipe
  { userId      :: ID User
  , name        :: Text
  , image       :: Maybe Image
  , description :: Maybe Text
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

--

data RecipeIngredient = RecipeIngredient
  { id           :: ID RecipeIngredient
  , recipeId     :: ID Recipe
  , ingredientId :: ID Ingredient
  , amount       :: Double
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

data NewRecipeIngredient = NewRecipeIngredient
  { recipeId     :: ID Recipe
  , ingredientId :: ID Ingredient
  , amount       :: Double
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

--

data RecipeInstruction = RecipeInstruction
  { id          :: ID RecipeInstruction
  , recipeId    :: ID Recipe
  , ordering    :: Word32
  , instruction :: Text
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

data NewRecipeInstruction = NewRecipeInstruction
  { recipeId    :: ID Recipe
  , ordering    :: Word32
  , instruction :: Text
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

--

data UserRecipe = UserRecipe
  { id       :: ID UserRecipe
  , userId   :: ID User
  , recipeId :: ID Recipe
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

data NewUserRecipe = NewUserRecipe
  { userId   :: ID User
  , recipeId :: ID Recipe
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )
