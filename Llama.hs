{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

module Llama where

import Conduit
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
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

newtype LlamaApplyTemplateRequest = LlamaApplyTemplateRequest
  { messages :: [LlamaMessage]
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

-- Llama request and response
data LlamaRequest = LlamaRequest
  { prompt :: Text
  , stream :: Bool
  } deriving (Show, Generic)
instance ToJSON LlamaRequest

newtype LlamaResponse = LlamaResponse
  { content :: Text
  } deriving (Show, Generic)
instance FromJSON LlamaResponse

type Token = Word32
type URL = String

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
  case decode (responseBody response) of
    Just (LlamaApplyTemplateResponse text) -> return (Just text)
    Nothing -> do
      liftIO $ hPutStrLn stderr $ "Failed to decode Llama response, got: " ++ (show $ responseBody response)
      return Nothing

-- Function to send a message to the Llama model
sendToLlama :: URL -> Manager -> Text -> IO (Maybe Text)
sendToLlama url manager input = do
  let request = parseRequest_ $ url ++ "/completion"
      body = encode (LlamaRequest input False)
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (LlamaResponse text) -> return (Just text)
    Nothing -> do
      liftIO $ hPutStrLn stderr $ "Failed to decode Llama response, got: " ++ (show $ responseBody response)
      return Nothing

sendToLlamaStreaming :: (MonadThrow m, MonadResource m) => URL -> Manager -> Text -> IO (ConduitT () LlamaStreamingResponse m ())
sendToLlamaStreaming url manager input = do
  let request = setRequestManager manager $ parseRequest_ $ url ++ "/completion"
      body = encode (LlamaRequest input True)
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
  case decode (responseBody response) of
    Just (LlamaTokenizeResponse result) -> return (Just result)
    Nothing -> do
      liftIO $ hPutStrLn stderr $ "Failed to decode Llama response, got: " ++ (show $ responseBody response)
      return Nothing

detokenize :: URL -> [Token] -> IO (Maybe Text)
detokenize url input = do
  let request = parseRequest_ $ url ++ "/detokenize"
      body = encode $ LlamaDetokenizeRequest input
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
  response <- httpLBS req
  case decode (responseBody response) of
    Just (LlamaDetokenizeResponse text) -> return (Just text)
    Nothing -> do
      liftIO $ hPutStrLn stderr $ "Failed to decode Llama response, got: " ++ (show $ responseBody response)
      return Nothing

llama :: URL -> Text -> IO (Maybe Text)
llama url input = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  sendToLlama url manager input

llamaTemplated :: URL -> LlamaApplyTemplateRequest -> IO (Maybe Text)
llamaTemplated url input = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  res <- applyTemplate url manager input
  case res of
    Just text -> sendToLlama url manager text
    _ -> pure Nothing

llamaTemplatedStreaming :: (MonadThrow m, MonadResource m) => URL -> LlamaApplyTemplateRequest -> IO (ConduitT () LlamaStreamingResponse m ())
llamaTemplatedStreaming url input = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  res <- applyTemplate url manager input
  case res of
    Just text -> sendToLlamaStreaming url manager text
    _ -> pure $ yieldMany []

health :: URL -> IO Health
health url = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  let request = parseRequest_ $ url ++ "/health"
  response <- httpLbs request manager
  pure $ if responseStatus response == ok200
       then HealthOk
       else HealthNok
