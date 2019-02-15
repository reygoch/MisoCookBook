{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
--
module Cookster.AppServer where
--
import Servant
import Data.Text                ( Text )
import Data.Proxy               ( Proxy (..) )
import Network.Wai.Handler.Warp ( run )
--

type API
  =             Get '[PlainText] Text
  :<|> "sub" :> Get '[PlainText] Text

api :: Proxy API
api = Proxy

apiServer :: Server API
apiServer = pure "hello" :<|> pure "kitty"

start :: IO ()
start = run 80 $ serve api apiServer
