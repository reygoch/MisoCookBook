{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
--
module Cookster.DataLayer.Model.Password where
--
import           Data.Text                         ( Text )
import           Data.Text.Encoding                ( encodeUtf8, decodeUtf8 )
import           Data.Aeson                        ( ToJSON, FromJSON )
import qualified GHC.Generics               as GHC ( Generic )
import qualified Generics.SOP               as SOP ( Generic, HasDatatypeInfo, K )
import           Generics.SOP.BasicFunctors        ( K (..) )
import           Squeal.PostgreSQL                 ( PG, PGType ( PGtext ) )
import           Squeal.PostgreSQL.Binary          ( FromValue (..), ToParam (..) )
import qualified Cookster.Settings          as S   ( Password (..) )
import           Crypto.BCrypt                     ( HashingPolicy (..), hashPasswordUsingPolicy )
import qualified Crypto.BCrypt              as BC  ( validatePassword )
import           Control.Monad.IO.Class            ( MonadIO (..) )
--

data Hash = Plain | Hashed
  deriving ( Eq, Show, GHC.Generic )

newtype Password ( h :: Hash ) = Password
  { unPassword :: Text
  } deriving
    ( Eq, Show
    , GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo
    , ToJSON, FromJSON )

type instance PG ( Password 'Hashed ) = PGtext

instance FromValue PGtext ( Password 'Hashed ) where
  fromValue = Password <$> fromValue @'PGtext

instance ToParam ( Password 'Hashed ) PGtext where
  toParam = toParam . unPassword

--

hashPassword
  :: MonadIO m
  => HashingPolicy
  -> Password 'Plain
  -> m ( Maybe ( Password 'Hashed ) )
hashPassword hp ( Password pp )
  =   liftIO ( hashPasswordUsingPolicy hp ( encodeUtf8 pp ) )
  >>= pure . fmap ( Password . decodeUtf8 )

validatePassword :: Password 'Hashed -> Password 'Plain -> Bool
validatePassword hashedPassword plainPassword
  = BC.validatePassword
    ( encodeUtf8 $ unPassword hashedPassword )
    ( encodeUtf8 $ unPassword plainPassword  )

makeHashingPolicy :: S.Password -> HashingPolicy
makeHashingPolicy p
  = HashingPolicy ( fromIntegral $ S.cost p ) ( encodeUtf8 $ S.algorithm p )
