{-# LANGUAGE OverloadedStrings #-}

module Llama where

import Network.HTTP.Conduit
import Data.Aeson
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStrLn, stderr)

-- Llama request and response
data LlamaRequest = LlamaRequest
  { prompt :: Text
  } deriving (Show)

data LlamaResponse = LlamaResponse
  { generatedText :: Text
  } deriving (Show)

instance FromJSON LlamaResponse where
  parseJSON = withObject "LlamaResponse" $ \v -> LlamaResponse
    <$> v .: "content"

instance ToJSON LlamaRequest where
  toJSON (LlamaRequest p) =
    object ["prompt" .= p]

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
