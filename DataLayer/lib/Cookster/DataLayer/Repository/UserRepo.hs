{-# LANGUAGE GADTs            #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
--
module Cookster.DataLayer.Repository.UserRepo where
--
import Data.Text                          ( Text )
import Control.Monad.Freer
import Squeal.PostgreSQL.Binary           ( Only (..) )
import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Pool
import Control.Monad.IO.Class             ( liftIO )
import Control.Monad.Trans.Control        ( MonadBaseControl )
import Cookster.DataLayer.Query
import Cookster.DataLayer.Model           ( Credentials, User (..) )
import Cookster.DataLayer.Model.Password  ( Hash (Hashed) )
import Cookster.DataLayer.Schema          ( Schema )
import Cookster.DataLayer.Database        ( DBPool, DB, doDB' )
--

data UserRepo a where
  CreateUser           :: Credentials 'Hashed -> UserRepo ( Maybe User )
  SelectUserByUsername :: Text -> UserRepo ( Maybe User )

createUser' :: Member UserRepo f => Credentials 'Hashed -> Eff f ( Maybe User )
createUser' = send . CreateUser

selectUserByUsername' :: Member UserRepo f => Text -> Eff f ( Maybe User )
selectUserByUsername' = send . SelectUserByUsername

runUserRepo :: Member DB f => Eff ( UserRepo ': f ) a -> Eff f a
runUserRepo = interpret $ doDB' . go
  where go :: UserRepo x -> PoolPQ Schema IO x
        go ( CreateUser c ) = manipulateParams createUser c >>= firstRow
        go ( SelectUserByUsername u ) = runQueryParams selectUserByUsername ( Only u ) >>= firstRow
