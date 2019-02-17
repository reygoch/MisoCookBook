{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Cookster.DataLayer.Database where
--
import Prelude             hiding ( unwords, concat )

import Data.Text.Encoding         ( encodeUtf8 )

import Data.ByteString            ( ByteString )
import Data.ByteString.Char8      ( unwords, concat )
import Data.ByteString.Conversion ( toByteString' )

import Control.Monad.IO.Class     ( MonadIO (..) )
import Control.Monad.Base         ( MonadBase )
import Control.Monad.Freer        ( Member, LastMember, Eff, send, interpretM )

import Squeal.PostgreSQL.PQ       ( Connection )
import Squeal.PostgreSQL.Pool     ( PoolPQ, Pool, createConnectionPool, destroyAllResources, runPoolPQ )
import Squeal.PostgreSQL.Schema   ( SchemaType )

import Generics.SOP.BasicFunctors ( K )

import Cookster.Settings          ( Database (..) )
import Cookster.DataLayer.Schema  ( Schema )
--

type DBPool ( schema :: SchemaType ) = Pool ( K Connection schema )

deletePool :: DBPool schema -> IO ()
deletePool = destroyAllResources

createPool :: MonadBase IO io => Database -> io ( DBPool schema )
createPool dbs = createConnectionPool
  ( createConnectionString  dbs )
  ( fromIntegral $ stripes  dbs )
  ( fromIntegral $ timeout  dbs )
  ( fromIntegral $ poolsize dbs )

createConnectionString :: Database -> ByteString
createConnectionString db = unwords $ fmap concat
  [ [ "port"    , "=", toByteString' $ port     db ]
  , [ "host"    , "=", encodeUtf8    $ host     db ]
  , [ "dbname"  , "=", encodeUtf8    $ name     db ]
  , [ "user"    , "=", encodeUtf8    $ username db ]
  , [ "password", "=", encodeUtf8    $ password db ]
  ]

data DB a where
  DoDB :: PoolPQ Schema IO a -> DB a

doDB' :: Member DB f => PoolPQ Schema IO a -> Eff f a
doDB' = send . DoDB

runDB :: LastMember IO f => DBPool Schema -> Eff ( DB ': f ) a -> Eff f a
runDB p = interpretM go
  where go :: DB x -> IO x
        go ( DoDB q ) = liftIO $ runPoolPQ q p
