module MyRest.Api where

{--
curl http://m.dev.mybetfirst.com:3000/newrest/1.0.0/message \
  -d '1' \
  -X POST \
  -H 'Content-Type: application/json'
--}

import Prelude
import Control.Monad.Reader
import Data.IORef
import Rest
import Rest.Api
import Rest.Driver.Wai
import System.Environment
import qualified Rest.Resource as R


silkApiExample st = apiToApplication (runApi st) api

api :: Api ApiState
api = [(mkVersion 1 0 0, Some1 router)]

router :: Router ApiState ApiState
router =
  root -/ route messageResource

type ApiState = ReaderT (IORef [Int]) IO

runApi :: IORef [Int] -> ApiState a -> IO a
runApi s b = runReaderT b s

initialState :: IO (IORef [Int])
initialState = newIORef [0]

messageResource :: Resource ApiState ApiState Int () Void
messageResource = mkResourceId
  { R.name   = "message"
  , R.schema = withListing () $ named []
  , R.list   = const getList
  , R.create = Just setMessage
  }

getList :: ListHandler ApiState
getList = mkListing (jsonO . someO) $ const (liftIO . readIORef =<< ask)

setMessage :: Handler ApiState
setMessage = mkInputHandler (jsonI . someI) $ \n -> do
  ref <- ask
  liftIO $ modifyIORef ref (n :)
