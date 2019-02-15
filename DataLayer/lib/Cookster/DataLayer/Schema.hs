{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
--
module Cookster.DataLayer.Schema where
--
import Data.Function ( (&) )
import Squeal.PostgreSQL
  ( PG, NullityType (..), SchemumType (..), PGType (..)
  , TableConstraint (..), ColumnConstraint (..), Definition (..)
  , OnDeleteClause (..), OnUpdateClause (..), Expression (..)
  , dropTable, createTable, dropType, createTypeEnumFrom
  , as, typedef, serial, int4, float8, text, timestamp
  , primaryKey, foreignKey, nullable, notNullable, check, unique, default_
  , (.>), (.==), (:::), (:=>), (>>>), NP (..)
  )
import Cookster.DataLayer.Model ( Unit )
--

type Schema =
  '[ "_Unit"       ::: 'Typedef ( PG Unit )
   , "_Ingredient" ::: 'Table
    (
    '[ "pk_Ingredient_id"        ::: 'PrimaryKey '[ "id" ]
     , "ck_Ingredient_cost_gt_0" ::: 'Check      '[ "cost" ]
     ] :=>

    '[ "id"           ::: 'Def   :=> 'NotNull 'PGint4
     , "name"         ::: 'NoDef :=> 'NotNull 'PGtext
     , "image"        ::: 'NoDef :=> 'NotNull 'PGtext
     , "description"  ::: 'NoDef :=> 'NotNull 'PGtext
     , "cost"         ::: 'NoDef :=> 'NotNull 'PGint4
     , "unit"         ::: 'NoDef :=> 'NotNull ( PG Unit )
     ]
    )
   , "_User" ::: 'Table
    (
      '[ "pk_User_id"       ::: 'PrimaryKey '[ "id" ]
       , "uq_User_username" ::: 'Unique '[ "username" ]
       ] :=>
      '[ "id"       ::: 'Def   :=> 'NotNull 'PGint4
       , "username" ::: 'NoDef :=> 'NotNull 'PGtext
       , "password" ::: 'NoDef :=> 'NotNull 'PGtext
       ]
    )
   , "_Recipe" ::: 'Table
    (
      '[ "pk_Recipe_id"          ::: 'PrimaryKey '[ "id" ]
       , "uq_Recipe_userId_name" ::: 'Unique     '[ "userId", "name" ]
       ] :=>
      '[ "id"          ::: 'Def   :=> 'NotNull 'PGint4
       , "userId"      ::: 'NoDef :=> 'NotNull 'PGint4
       , "name"        ::: 'NoDef :=> 'NotNull 'PGtext
       , "image"       ::: 'NoDef :=> 'Null    'PGtext
       , "description" ::: 'NoDef :=> 'Null    'PGtext
       ]
    )
   , "_RecipeIngredient" ::: 'Table
    (
      '[ "pk_RecipeIngredient_id"                    ::: 'PrimaryKey '[ "id" ]
       , "uq_RecipeIngredient_recipeId_ingredientId" ::: 'Unique     '[ "recipeId", "ingredientId" ]
       , "ck_RecipeIngredient_amount_gt_0"           ::: 'Check      '[ "amount" ]
       , "fk_RecipeIngredient_recipeId"              ::: 'ForeignKey '[ "recipeId" ]     "_Recipe"     '[ "id" ]
       , "fk_RecipeIngredient_ingredientId"          ::: 'ForeignKey '[ "ingredientId" ] "_Ingredient" '[ "id" ]
       ] :=>
      '[ "id"           ::: 'Def   :=> 'NotNull 'PGint4
       , "recipeId"     ::: 'NoDef :=> 'NotNull 'PGint4
       , "ingredientId" ::: 'NoDef :=> 'NotNull 'PGint4
       , "amount"       ::: 'NoDef :=> 'NotNull 'PGfloat8
       ]
    )
   , "_RecipeInstruction" ::: 'Table
    (
      '[ "pk_RecipeInstruction_id"       ::: 'PrimaryKey '[ "id" ]
       , "fk_RecipeInstruction_recipeId" ::: 'ForeignKey '[ "recipeId" ] "_Recipe" '[ "id" ]
       ] :=>
      '[ "id"          ::: 'Def   :=> 'NotNull 'PGint4
       , "recipeId"    ::: 'NoDef :=> 'NotNull 'PGint4
       , "ordering"    ::: 'NoDef :=> 'NotNull 'PGint4
       , "instruction" ::: 'NoDef :=> 'NotNull 'PGtext
       ]
    )
   , "_UserRecipe" ::: 'Table
    (
      '[ "pk_UserRecipe_id"              ::: 'PrimaryKey '[ "id" ]
       , "fk_UserRecipe_userId"          ::: 'ForeignKey '[ "userId" ]   "_User"   '[ "id" ]
       , "fk_UserRecipe_recipeId"        ::: 'ForeignKey '[ "recipeId" ] "_Recipe" '[ "id" ]
       , "uq_UserRecipe_userId_recipeId" ::: 'Unique     '[ "userId", "recipeId" ]
       ] :=>
      '[ "id"       ::: 'Def   :=> 'NotNull 'PGint4
       , "userId"   ::: 'NoDef :=> 'NotNull 'PGint4
       , "recipeId" ::: 'NoDef :=> 'NotNull 'PGint4
       ]
    )
   , "_Generation" ::: 'Table
    (
      '[ "pk_Generation_id"        ::: 'PrimaryKey '[ "id" ]
       , "uq_Generation_timestamp" ::: 'Unique     '[ "timestamp" ]
       ] :=>
      '[ "id"        ::: 'Def :=> 'NotNull 'PGint4
       , "timestamp" ::: 'Def :=> 'NotNull 'PGtimestamp
       ]
    )
   , "_Cluster" ::: 'Table
    (
      '[ "pk_Cluster_id"           ::: 'PrimaryKey '[ "id" ]
       , "fk_Cluster_generationId" ::: 'ForeignKey '[ "generationId" ] "_Generation" '[ "id" ]
       ] :=>
      '[ "id"           ::: 'Def   :=> 'NotNull 'PGint4
       , "generationId" ::: 'NoDef :=> 'NotNull 'PGint4
       ]
    )
   , "_UserCluster" ::: 'Table
    (
      '[ "pk_UserCluster_id"               ::: 'PrimaryKey '[ "id" ]
       , "fk_UserCluster_userId"           ::: 'ForeignKey '[ "userId" ] "_User" '[ "id" ]
       , "fk_UserCluster_clusterId"        ::: 'ForeignKey '[ "clusterId" ] "_Cluster" '[ "id" ]
       , "uq_UserCluster_userId_clusterId" ::: 'Unique     '[ "userId", "clusterId" ]
       ] :=>
      '[ "id"        ::: 'Def   :=> 'NotNull 'PGint4
       , "userId"    ::: 'NoDef :=> 'NotNull 'PGint4
       , "clusterId" ::: 'NoDef :=> 'NotNull 'PGint4
       ]
    )
   ]

schemaTeardown :: Definition Schema '[]
schemaTeardown
  =   dropTable #_RecipeIngredient
  >>> dropTable #_Ingredient
  >>> dropTable #_RecipeInstruction
  >>> dropTable #_UserRecipe
  >>> dropTable #_Recipe
  >>> dropTable #_UserCluster
  >>> dropTable #_User
  >>> dropTable #_Cluster
  >>> dropTable #_Generation
  >>> dropType  #_Unit

schemaCreation :: Definition '[] Schema
schemaCreation
  =   createTypeEnumFrom @Unit #_Unit
  >>> createTable #_Ingredient
      (  serial                           `as` #id
      :* ( text           & notNullable ) `as` #name
      :* ( text           & notNullable ) `as` #image
      :* ( text           & notNullable ) `as` #description
      :* ( int4           & notNullable ) `as` #cost
      :* ( typedef #_Unit & notNullable ) `as` #unit
      )
      (  primaryKey #id                  `as` #pk_Ingredient_id
      :* check      #cost ( #cost .> 0 ) `as` #ck_Ingredient_cost_gt_0
      )
  >>> createTable #_User
      (  serial                 `as` #id
      :* ( text & notNullable ) `as` #username
      :* ( text & notNullable ) `as` #password
      )
      (  primaryKey #id       `as` #pk_User_id
      :* unique     #username `as` #uq_User_username
      )
  >>> createTable #_Recipe
      (  serial                 `as` #id
      :* ( int4 & notNullable ) `as` #userId
      :* ( text & notNullable ) `as` #name
      :* ( text & nullable    ) `as` #image
      :* ( text & nullable    ) `as` #description
      )
      (  primaryKey #id                  `as` #pk_Recipe_id
      :* unique     ( #userId :* #name ) `as` #uq_Recipe_userId_name
      )
  >>> createTable #_RecipeIngredient
      (  serial `as` #id
      :* ( int4   & notNullable ) `as` #recipeId
      :* ( int4   & notNullable ) `as` #ingredientId
      :* ( float8 & notNullable ) `as` #amount
      )
      (  primaryKey #id                                                                `as` #pk_RecipeIngredient_id
      :* unique     ( #recipeId :* #ingredientId )                                     `as` #uq_RecipeIngredient_recipeId_ingredientId
      :* check      #amount       ( #amount .> 0 )                                     `as` #ck_RecipeIngredient_amount_gt_0
      :* foreignKey #recipeId     #_Recipe         #id OnDeleteCascade OnUpdateCascade `as` #fk_RecipeIngredient_recipeId
      :* foreignKey #ingredientId #_Ingredient     #id OnDeleteCascade OnUpdateCascade `as` #fk_RecipeIngredient_ingredientId
      )
  >>> createTable #_RecipeInstruction
      (  serial                 `as` #id
      :* ( int4 & notNullable ) `as` #recipeId
      :* ( int4 & notNullable ) `as` #ordering
      :* ( text & notNullable ) `as` #instruction
      )
      (  primaryKey #id                                                    `as` #pk_RecipeInstruction_id
      :* foreignKey #recipeId #_Recipe #id OnDeleteCascade OnUpdateCascade `as` #fk_RecipeInstruction_recipeId
      )
  >>> createTable #_UserRecipe
      (  serial                 `as` #id
      :* ( int4 & notNullable ) `as` #userId
      :* ( int4 & notNullable ) `as` #recipeId
      )
      (  primaryKey #id                                                    `as` #pk_UserRecipe_id
      :* foreignKey #userId   #_User   #id OnDeleteCascade OnUpdateCascade `as` #fk_UserRecipe_userId
      :* foreignKey #recipeId #_Recipe #id OnDeleteCascade OnUpdateCascade `as` #fk_UserRecipe_recipeId
      :* unique     ( #userId :* #recipeId )                               `as` #uq_UserRecipe_userId_recipeId
      )
  >>> createTable #_Generation
      (  serial                                                              `as` #id
      :* ( timestamp & notNullable & default_ ( UnsafeExpression "now()" ) ) `as` #timestamp
      )
      (  primaryKey #id        `as` #pk_Generation_id
      :* unique     #timestamp `as` #uq_Generation_timestamp
      )
  >>> createTable #_Cluster
      (  serial                 `as` #id
      :* ( int4 & notNullable ) `as` #generationId
      )
      (  primaryKey #id                                                            `as` #pk_Cluster_id
      :* foreignKey #generationId #_Generation #id OnDeleteCascade OnUpdateCascade `as` #fk_Cluster_generationId
      )
  >>> createTable #_UserCluster
      (  serial                 `as` #id
      :* ( int4 & notNullable ) `as` #userId
      :* ( int4 & notNullable ) `as` #clusterId
      )
      (  primaryKey #id                                                      `as` #pk_UserCluster_id
      :* foreignKey #userId    #_User    #id OnDeleteCascade OnUpdateCascade `as` #fk_UserCluster_userId
      :* foreignKey #clusterId #_Cluster #id OnDeleteCascade OnUpdateCascade `as` #fk_UserCluster_clusterId
      :* unique ( #userId :* #clusterId )                                    `as` #uq_UserCluster_userId_clusterId
      )
