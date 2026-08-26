{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

module Llama where

import Conduit
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Simple hiding (httpLbs)
import Network.HTTP.Types.Status
import System.IO (hPutStrLn, stderr)

import Llama.Streaming

data Role = System | User | CustomRole Text deriving Show
instance ToJSON Role where
  toJSON System = "system"
  toJSON User = "user"
  toJSON (CustomRole t) = String t

data LlamaMessage = LlamaMessage
  { role :: Role
  , content :: Text
  } deriving (Show, Generic)
instance ToJSON LlamaMessage

data LlamaApplyTemplateRequest = LlamaApplyTemplateRequest
  { messages :: [LlamaMessage]
  , model :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON LlamaApplyTemplateRequest

newtype LlamaApplyTemplateResponse = LlamaApplyTemplateResponse
  { prompt :: Text
  } deriving (Show, Generic)
instance FromJSON LlamaApplyTemplateResponse

data LlamaTokenizeRequest = LlamaTokenizeRequest
  { content :: Text
  , add_special :: Bool
  , parse_special :: Bool
  } deriving (Show, Generic)
instance ToJSON LlamaTokenizeRequest

newtype LlamaTokenizeResponse = LlamaTokenizeResponse
  { tokens :: [Token]
  } deriving (Show, Generic)
instance FromJSON LlamaTokenizeResponse

newtype LlamaDetokenizeRequest = LlamaDetokenizeRequest
  { tokens :: [Token]
  } deriving (Show, Generic)
instance ToJSON LlamaDetokenizeRequest

newtype LlamaDetokenizeResponse = LlamaDetokenizeResponse
  { content :: Text
  } deriving (Show, Generic)
instance FromJSON LlamaDetokenizeResponse

data Health = HealthOk | HealthNok deriving (Show)

-- llama.cpp rejects requests with null options since https://github.com/ggml-org/llama.cpp/pull/24150
noMaybes = defaultOptions { omitNothingFields = True }
-- Llama request and response
data LlamaRequest = LlamaRequest
  { prompt :: Text
  , stream :: Bool
  , cache_prompt :: Maybe Bool
  , model :: Maybe Text
  } deriving (Show, Generic)
instance ToJSON LlamaRequest where
  toJSON = genericToJSON noMaybes
  toEncoding = genericToEncoding noMaybes
instance Default LlamaRequest where
  def = LlamaRequest
      { prompt = ""
      , stream = False
      , cache_prompt = Nothing
      , model = Nothing
      }

newtype LlamaResponse = LlamaResponse
  { content :: Text
  } deriving (Show, Generic)
instance FromJSON LlamaResponse

data LlamaError = LlamaError
  { code :: Word
  , message :: Text
  } deriving (Show, Generic)
instance FromJSON LlamaError

newtype LlamaResponseError = LlamaResponseError
  { error :: LlamaError
  } deriving (Show, Generic)
instance FromJSON LlamaResponseError

type Token = Word32
type URL = String

llamaDecode :: (FromJSON a) => ByteString -> IO (Maybe a)
llamaDecode x =
  case decode x of
    Just v -> return v
    Nothing -> do
      case decode x of
        Just (LlamaResponseError err) -> do
          liftIO $ hPutStrLn stderr $ "llama-server returned an error: " ++ show err
        Nothing -> do
          liftIO $ hPutStrLn stderr $ "Failed to decode Llama response, got: " ++ show x
      return Nothing

-- |Apply the LLM tempate to produce a raw LLM prompt from the role-content pairs
applyTemplateSimple :: URL -> LlamaApplyTemplateRequest -> IO (Maybe Text)
applyTemplateSimple = applyTemplateGeneral httpLBS

-- |Like `applyTemplateSimple` but with user-supplied `Manager`
applyTemplate :: URL -> Manager -> LlamaApplyTemplateRequest -> IO (Maybe Text)
applyTemplate url manager = applyTemplateGeneral (`httpLbs` manager) url

-- |Like `applyTemplateSimple` but with user-supplied fetcher
applyTemplateGeneral :: (ToJSON p) => (Request -> IO (Response ByteString)) -> [Char] -> p -> IO (Maybe Text)
applyTemplateGeneral fetch url input = do
  let request = parseRequest_ $ url ++ "/apply-template"
      body = encode input
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
  response <- fetch req
  decoded <- llamaDecode (responseBody response)
  return $ decoded >>= (\(LlamaApplyTemplateResponse text) -> Just text)

-- |Simple completion API
sendToLlama :: URL -> Manager -> Text -> IO (Maybe Text)
sendToLlama url manager input = sendToLlamaRequest url manager (def { prompt = input })

-- |Allows to specify other completion options
sendToLlamaRequest :: URL -> Manager -> LlamaRequest -> IO (Maybe Text)
sendToLlamaRequest url manager lreq = do
  let request = parseRequest_ $ url ++ "/completion"
      body = encode lreq
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/json")]
                    , responseTimeout = responseTimeoutMicro 1800000000
                    }
  response <- httpLbs req manager
  decoded <- llamaDecode (responseBody response)
  return $ decoded >>= (\(LlamaResponse text) -> Just text)

-- |Returns a token-by-token stream
sendToLlamaStreaming :: (MonadThrow m, MonadResource m) => URL -> Manager -> Text -> IO (ConduitT () LlamaStreamingResponse m ())
sendToLlamaStreaming url manager input = sendToLlamaStreamingRequest url manager (def { prompt = input })

-- |Allows to specify other completion options
sendToLlamaStreamingRequest :: (MonadThrow m, MonadResource m) => URL -> Manager -> LlamaRequest -> IO (ConduitT () LlamaStreamingResponse m ())
sendToLlamaStreamingRequest url manager lreq = do
  let request = setRequestManager manager $ parseRequest_ $ url ++ "/completion"
      body = encode (lreq { stream = True })
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
  pure $ httpSource req getResponseBody .| eventConduit

tokenize :: URL -> LlamaTokenizeRequest -> IO (Maybe [Token])
tokenize url input = do
  let request = parseRequest_ $ url ++ "/tokenize"
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS $ encode input
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
  response <- httpLBS req
  decoded <- llamaDecode (responseBody response)
  return $ decoded >>= (\(LlamaTokenizeResponse result) -> Just result)

detokenize :: URL -> [Token] -> IO (Maybe Text)
detokenize url input = do
  let request = parseRequest_ $ url ++ "/detokenize"
      body = encode $ LlamaDetokenizeRequest input
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
  response <- httpLBS req
  decoded <- llamaDecode (responseBody response)
  return $ decoded >>= (\(LlamaDetokenizeResponse result) -> Just result)

-- |Extremely basic interface
llama :: URL -> Text -> IO (Maybe Text)
llama url input = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  sendToLlama url manager input

-- |Uses `applyTemplate` before sending the completion request
llamaTemplated :: URL -> LlamaApplyTemplateRequest -> IO (Maybe Text)
llamaTemplated url input = llamaTemplatedRequest url input def

-- |Make sure to use the same model in both `LlamaApplyTemplateRequest` and `LlamaRequest`
llamaTemplatedRequest :: URL -> LlamaApplyTemplateRequest -> LlamaRequest -> IO (Maybe Text)
llamaTemplatedRequest url input lreq = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  res <- applyTemplate url manager input
  case res of
    Just text -> sendToLlamaRequest url manager lreq { prompt = text }
    _ -> pure Nothing

llamaTemplatedStreaming :: (MonadThrow m, MonadResource m) => URL -> LlamaApplyTemplateRequest -> IO (ConduitT () LlamaStreamingResponse m ())
llamaTemplatedStreaming url input = llamaTemplatedStreamingRequest url input def

-- |Make sure to use the same model in both `LlamaApplyTemplateRequest` and `LlamaRequest`
llamaTemplatedStreamingRequest :: (MonadThrow m, MonadResource m) => URL -> LlamaApplyTemplateRequest -> LlamaRequest -> IO (ConduitT () LlamaStreamingResponse m ())
llamaTemplatedStreamingRequest url input lreq = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  res <- applyTemplate url manager input
  case res of
    Just text -> sendToLlamaStreamingRequest url manager lreq { prompt = text }
    _ -> pure $ yieldMany []

health :: URL -> IO Health
health url = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  let request = parseRequest_ $ url ++ "/health"
  response <- httpLbs request manager
  pure $ if responseStatus response == ok200
       then HealthOk
       else HealthNok
