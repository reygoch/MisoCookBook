{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Cookster.Api where
--
import Servant
import Cookster.DataLayer.Model
--

type IngredientAPI
  = Get '[ JSON ] [ Ingredient ]

ingredientAPI :: Proxy IngredientAPI
ingredientAPI = Proxy

ingredientServer :: Server IngredientAPI
ingredientServer = pure [ Ingredient ( ID 1 ) "Ing1" ( Image "Ing1.jpg" ) "Some description" ( Cost 10000 ) Kg ]
