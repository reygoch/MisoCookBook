{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeInType        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Cookster.DataLayer.Database where
--
import Prelude             hiding ( unwords, concat )
import Data.Text.Encoding         ( encodeUtf8 )
import Data.ByteString            ( ByteString )
import Data.ByteString.Char8      ( unwords, concat )
import Data.ByteString.Conversion ( toByteString' )
import Control.Monad.Base         ( MonadBase )
import Squeal.PostgreSQL.PQ       ( Connection )
import Squeal.PostgreSQL.Pool     ( Pool, createConnectionPool )
import Squeal.PostgreSQL.Schema   ( SchemaType )
import Generics.SOP.BasicFunctors ( K )
import Cookster.Settings          ( Database (..) )
--

type DBPool ( schema :: SchemaType ) = Pool ( K Connection schema )

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
