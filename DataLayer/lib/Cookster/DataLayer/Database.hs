{-# LANGUAGE FlexibleContexts #-}
--
module Cookster.DataLayer.Database where
--
import Data.ByteString            ( ByteString )
import Control.Monad.Base         ( MonadBase )
import Squeal.PostgreSQL.PQ       ( Connection )
import Squeal.PostgreSQL.Pool     ( Pool, createConnectionPool )
import Generics.SOP.BasicFunctors ( K )
import Cookster.Settings          ( Database (..) )
--

createPool :: MonadBase IO io => Database -> io ( Pool ( K Connection schema ) )
createPool dbs = createConnectionPool
  ( createConnectionString  dbs )
  ( fromIntegral $ stripes  dbs )
  ( fromIntegral $ timeout  dbs )
  ( fromIntegral $ poolsize dbs )

createConnectionString :: Database -> ByteString
createConnectionString = undefined
