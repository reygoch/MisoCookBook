{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
--
module Cookster.Settings where
--
import Data.Text              ( Text )
import Data.Yaml              ( ParseException, decodeFileEither )
import Data.Aeson             ( FromJSON (..) )

import GHC.Generics           ( Generic )
import Control.Monad.IO.Class ( MonadIO, liftIO )
--

readSettings :: MonadIO m => FilePath -> m ( Either ParseException Settings )
readSettings = liftIO . decodeFileEither

--

data Settings = Settings
  { server   :: Server
  , database :: Database
  , password :: Password
  } deriving ( Eq, Show, Generic )

instance FromJSON Settings

--

data Server = Server
  { port :: Word     -- ^ port on which to listen for requests
  , jwkf :: FilePath -- ^ JSON Web Token File
  } deriving ( Eq, Show, Generic )

instance FromJSON Server

--

data Database = Database
  { port     :: Word -- ^ port on which the database is listening for queries
  , host     :: Text -- ^ name of the database host
  , name     :: Text -- ^ name of the database
  , timeout  :: Word -- ^ how long the unused connection is kept open
  , poolsize :: Word -- ^ how many connections are in the connection pool
  , username :: Text -- ^ username for the database user
  , password :: Text -- ^ password for the database user
  } deriving ( Eq, Show, Generic )

instance FromJSON Database

--

data Password = Password
  { cost      :: Word -- ^ cost of a hashing algorithm function
  , algorithm :: Text -- ^ hashing algorithm to use (hashing library is BCrypt)
  } deriving ( Eq, Show, Generic )

instance FromJSON Password
