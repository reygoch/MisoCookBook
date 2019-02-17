{-# LANGUAGE GADTs            #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
--
module Cookster.DataLayer.Repository.IngredientRepo where
--
import Control.Monad.Freer
import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Pool
import Control.Monad.IO.Class      ( liftIO )
import Control.Monad.Trans.Control ( MonadBaseControl )
import Cookster.DataLayer.Query
import Cookster.DataLayer.Model    ( Ingredient (..) )
import Cookster.DataLayer.Schema   ( Schema )
import Cookster.DataLayer.Database ( DBPool, DB, doDB' )
--

data IngredientRepo a where
  SelectAllIngredients :: IngredientRepo [ Ingredient ]

selectAllIngredients' :: Member IngredientRepo f => Eff f [ Ingredient ]
selectAllIngredients' = send SelectAllIngredients

runIngredientRepo :: Member DB f => Eff ( IngredientRepo ': f ) a -> Eff f a
runIngredientRepo = interpret $ doDB' . go
  where go :: IngredientRepo x -> PoolPQ Schema IO x
        go SelectAllIngredients = runQuery selectAllIngredients >>= getRows
