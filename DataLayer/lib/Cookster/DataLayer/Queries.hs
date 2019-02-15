{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
--
module Cookster.DataLayer.Queries where
--
import Squeal.PostgreSQL
import Cookster.DataLayer.Schema ( Schema )
import Cookster.DataLayer.Model
import Cookster.DataLayer.Model.Password ( Hash (..) )
--

createUser :: Manipulation Schema ( TuplePG ( Credentials 'Hashed ) ) ( RowPG User )
createUser = insertRow #_User
  (  Default          `as` #id
  :* Set ( param @1 ) `as` #username
  :* Set ( param @2 ) `as` #password
  )
  OnConflictDoRaise
  ( Returning $ #id :* #username :* #password )

selectUserByUsername :: Query Schema '[ 'NotNull 'PGtext ] ( RowPG User )
selectUserByUsername
  = selectStar
  $ from ( table #_User )
  & where_ ( #username .== param @1 )

--

createIngredient :: Manipulation Schema ( TuplePG NewIngredient ) ( RowPG Ingredient )
createIngredient = insertRow #_Ingredient
  (  Default          `as` #id
  :* Set ( param @1 ) `as` #name
  :* Set ( param @2 ) `as` #image
  :* Set ( param @3 ) `as` #description
  :* Set ( param @4 ) `as` #cost
  :* Set ( param @5 ) `as` #unit
  )
  OnConflictDoRaise
  ( Returning $ #id :* #name :* #image :* #description :* #cost :* #unit )

selectIngredientById :: Query Schema ( TuplePG ( ID Ingredient ) ) ( RowPG Ingredient )
selectIngredientById
  = selectStar
  $ from   ( table #_Ingredient )
  & where_ ( #id .== param @1 )

selectIngredientByName :: Query Schema '[ 'NotNull 'PGtext ] ( RowPG Ingredient )
selectIngredientByName
  = selectStar
  $ from   ( table #_Ingredient )
  & where_ ( #name .== param @1 )

selectAllIngredients :: Query Schema p ( RowPG Ingredient )
selectAllIngredients
  = selectStar
  $ from ( table #_Ingredient )

selectPaginatedIngredients :: Pagination -> Query Schema '[] ( RowPG Ingredient )
selectPaginatedIngredients p
  = selectStar
  $ from   ( table #_Ingredient )
  & limit  ( items p )
  & offset ( items p * pagen p )

--

createRecipe :: Manipulation Schema ( TuplePG NewRecipe ) ( RowPG Recipe )
createRecipe = insertRow #_Recipe
  (  Default          `as` #id
  :* Set ( param @1 ) `as` #userId
  :* Set ( param @2 ) `as` #name
  :* Set ( param @3 ) `as` #image
  :* Set ( param @4 ) `as` #description
  )
  OnConflictDoRaise
  ( Returning $ #id :* #userId :* #name :* #image :* #description )

createRecipeIngredient :: Manipulation Schema ( TuplePG NewRecipeIngredient ) ( RowPG RecipeIngredient )
createRecipeIngredient = insertRow #_RecipeIngredient
  (  Default          `as` #id
  :* Set ( param @1 ) `as` #recipeId
  :* Set ( param @2 ) `as` #ingredientId
  :* Set ( param @3 ) `as` #amount
  )
  OnConflictDoRaise
  ( Returning $ #id :* #recipeId :* #ingredientId :* #amount )

createRecipeInstruction :: Manipulation Schema ( TuplePG NewRecipeInstruction ) ( RowPG RecipeInstruction )
createRecipeInstruction = insertRow #_RecipeInstruction
  (  Default          `as` #id
  :* Set ( param @1 ) `as` #recipeId
  :* Set ( param @2 ) `as` #ordering
  :* Set ( param @3 ) `as` #instruction
  )
  OnConflictDoRaise
  ( Returning $ #id :* #recipeId :* #ordering :* #instruction )

createUserRecipe :: Manipulation Schema ( TuplePG NewUserRecipe ) ( RowPG UserRecipe )
createUserRecipe = insertRow #_UserRecipe
  (  Default `as` #id
  :* Set ( param @1 ) `as` #userId
  :* Set ( param @2 ) `as` #recipeId
  )
  OnConflictDoRaise
  ( Returning $ #id :* #userId :* #recipeId )
