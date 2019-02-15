module Cookster.DevServer where
--
import           GHC.Word                        ( Word32 )
import           Data.IORef                      ( IORef, newIORef, readIORef, writeIORef )
import           Foreign.Store                   ( Store (..), lookupStore, readStore, storeAction, withStore )

import           Control.Monad                   ( (>=>) )
import           Control.Concurrent              ( ThreadId, MVar, newEmptyMVar, putMVar, takeMVar, killThread, forkFinally )

import qualified Cookster.AppServer as AppServer ( start )
--

{- |
  Index of the 'Store' for storing our web 'server's 'ThreadId'.
-}
threadIdStoreNum :: Word32
threadIdStoreNum = 1

{- |
  This function will start or restart the application 'server' on subsequent
  code changes. It should be used in conjunction with @ghcid@ and is run by the
  following command:

  @
  ghcid --command "stack ghci cookster" --test "Cookster.DevServer.update"
  @
-}
start :: IO ()
start = do
  -- Check if there is a currently running application by trying to get a
  -- 'ThreadId' 'Store'.
  mThreadIdStore <- lookupStore threadIdStoreNum

  case mThreadIdStore of
    -- If 'ThreadId' store doesn't exists:
    Nothing -> do
      -- Create new empty dummy store which will be used to signal when the next
      -- thread can be started (I think...).
      done <- storeAction doneStore newEmptyMVar
      -- Pass it to the function wich starts the server for the first time in a
      -- new thread and returns the 'ThreadId'.
      tid  <- startServer done
      -- Store that 'ThreadId' into the 'Store'.
      _    <- storeAction ( Store threadIdStoreNum ) ( newIORef tid )
      -- Complete the 'update' while the server is running safely in another
      -- thread.
      pure ()
    -- If 'ThreadId' store does exist:
    Just threadIdStore ->
      restartServer threadIdStore
  where
    doneStore :: Store ( MVar () )
    doneStore = Store 0

    startServer :: MVar () -> IO ThreadId
    startServer done = forkFinally AppServer.start ( \_ -> putMVar done () )

    restartServer :: Store ( IORef ThreadId ) -> IO ()
    restartServer threadIdStore = modifyStoredIORef threadIdStore $ \tid -> do
      killThread tid
      withStore doneStore takeMVar
      readStore doneStore >>= startServer

{- |
  Utility function for manually shutting down a dev server, killing the
  thread and all that.
-}
shutdown :: IO ()
shutdown = do
  mThreadIdStore <- lookupStore threadIdStoreNum
  case mThreadIdStore of
    Nothing ->
      putStrLn "cookster: no server running"
    Just threadIdStore -> do
      withStore threadIdStore $ readIORef >=> killThread
      putStrLn "cookster: server is shutdown"

{- |
  Utility function for modifying stored (from the 'Store') 'IORef' values.
-}
modifyStoredIORef :: Store ( IORef a ) -> ( a -> IO a ) -> IO ()
modifyStoredIORef store f = withStore store $ \ref ->
  readIORef ref >>= f >>= writeIORef ref
