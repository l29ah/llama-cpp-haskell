{-
Copyright Kadena LLC (c) 2019

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Llama.Streaming
  ( eventConduit
  , LlamaStreamingResponse(..)
  )
where

import Control.Applicative
import Control.Monad.Catch
import Data.Aeson
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 as AC8
import Data.ByteString as B
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Text
import GHC.Generics

data ServerEvent = ServerEvent
  { eventName :: Maybe ByteString
  , eventId   :: Maybe ByteString
  , eventData :: [ByteString]
  } | CommentEvent
  { eventComment :: ByteString
  } | RetryEvent
  { eventRetry :: Int
  } | CloseEvent
  deriving (Show)

data LlamaStreamingResponse = LlamaStreamingResponse
  { index :: Int
  , content :: Text
  , stop :: Bool
  , tokens_predicted :: Int
  , tokens_evaluated :: Int
  } deriving (Show, Generic)
instance FromJSON LlamaStreamingResponse

eventConduit :: (MonadThrow m) => ConduitT ByteString LlamaStreamingResponse m ()
eventConduit = mapOutputMaybe parseEventJSON $ conduitParser event

parseEventJSON :: (PositionRange, ServerEvent) -> Maybe LlamaStreamingResponse
parseEventJSON (_, ServerEvent _ _ [d]) = decodeStrict d
parseEventJSON _ = Nothing

-- |text/event-stream parser
event :: Parser ServerEvent
event = (sevent <|> comment <|> retry) <* eol

sevent :: Parser ServerEvent
sevent = ServerEvent
  <$> optional (string "event" *> char ':' *> chars <* eol)
  <*> optional (string "id"    *> char ':' *> chars <* eol)
  <*> many     (string "data"  *> char ':' *> chars <* eol)

comment :: Parser ServerEvent
comment = CommentEvent <$> (char ':' *> chars <* eol)

retry :: Parser ServerEvent
retry = RetryEvent <$> (string "retry:" *> decimal <* eol)

chars :: Parser ByteString
chars = AC8.takeTill (== '\n')

eol :: Parser Char
eol = char '\n'
