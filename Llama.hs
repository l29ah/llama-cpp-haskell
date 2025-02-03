{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Llama where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Conduit
import System.IO (hPutStrLn, stderr)

data Role = System | User | CustomRole Text deriving Show
instance ToJSON Role where
  toJSON System = "system"
  toJSON User = "system"
  toJSON (CustomRole t) = String t

data LlamaMessage = LlamaMessage
  { role :: Role
  , content :: Text
  } deriving (Show)

instance ToJSON LlamaMessage where
  toJSON m =
    object
      [ "role" .= role m
      , "content" .= content m
      ]

newtype LlamaApplyTemplateRequest = LlamaApplyTemplateRequest
  { messages :: [LlamaMessage]
  } deriving (Show, Generic)
instance ToJSON LlamaApplyTemplateRequest

--data LlamaApplyTemplateResponse = LlamaApplyTemplateResponse
--  { prompt :: Text
--  } deriving (Show)

-- Llama request and response
newtype LlamaRequest = LlamaRequest
  { prompt :: Text
  } deriving (Show)

instance FromJSON LlamaRequest where
  parseJSON = withObject "LlamaRequest" $ \v -> LlamaRequest
    <$> v .: "prompt"

instance ToJSON LlamaRequest where
  toJSON (LlamaRequest p) =
    object ["prompt" .= p]

newtype LlamaResponse = LlamaResponse
  { generatedText :: Text
  } deriving (Show)

instance FromJSON LlamaResponse where
  parseJSON = withObject "LlamaResponse" $ \v -> LlamaResponse
    <$> v .: "content"

type URL = String

applyTemplate :: URL -> Manager -> LlamaApplyTemplateRequest -> IO (Maybe Text)
applyTemplate url manager input = do
  let request = parseRequest_ $ url ++ "/apply-template"
      body = encode input
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (LlamaRequest text) -> return (Just text)
    Nothing -> do
      liftIO $ hPutStrLn stderr "Failed to decode Llama response"
      return Nothing

-- Function to send a message to the Llama model
sendToLlama :: URL -> Manager -> Text -> IO (Maybe Text)
sendToLlama url manager input = do
  let request = parseRequest_ $ url ++ "/completion"
      body = encode (LlamaRequest input)
      req = request { method = "POST"
                    , requestBody = RequestBodyLBS body
                    , requestHeaders = [("Content-Type", "application/json")]
                    }
  response <- httpLbs req manager
  case decode (responseBody response) of
    Just (LlamaResponse text) -> return (Just text)
    Nothing -> do
      liftIO $ hPutStrLn stderr "Failed to decode Llama response"
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
