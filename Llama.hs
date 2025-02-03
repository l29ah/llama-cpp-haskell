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

applyTemplate :: Manager -> LlamaApplyTemplateRequest -> IO (Maybe Text)
applyTemplate manager input = do
  let request = parseRequest_ "http://localhost:8080/apply-template"
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
sendToLlama :: Manager -> Text -> IO (Maybe Text)
sendToLlama manager input = do
  let request = parseRequest_ "http://localhost:8080/completion"
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

llama :: Text -> IO (Maybe Text)
llama input = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  sendToLlama manager input

llamaTemplated :: LlamaApplyTemplateRequest -> IO (Maybe Text)
llamaTemplated input = do
  manager <- liftIO $ newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  res <- applyTemplate manager input
  case res of
    Just text -> sendToLlama manager text
    _ -> pure Nothing
