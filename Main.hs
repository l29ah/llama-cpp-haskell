{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Conduit
import Data.Conduit.Lazy
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Generic
import System.Exit
import System.IO

import Llama hiding (model)
import qualified Llama
import Llama.Streaming as LS

data Options w = Options
  { systemPrompt :: w ::: Text <?> "The system prompt to use" <!> "You are a helpful assistant."
  , url :: w ::: String <?> "llama-server URL" <!> "http://localhost:8080"
  , streaming :: w ::: Bool <?> "use to stream output from the LLM"
  , stripThinking :: w ::: Bool <?> "remove \"</think>\" and everything that occurs before it"
  , templateOnly :: w ::: Bool <?> "only apply the chat template without running LLM completion"
  , model :: w ::: Maybe Text <?> "model to use"
  } deriving (Generic)

instance ParseRecord (Options Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  opts <- unwrapRecord "A command line interface for llama-server"
  input <- T.getContents
  let request = [ LlamaMessage System $ systemPrompt opts
                , LlamaMessage User input
                ]
  if templateOnly opts then do
    response <- applyTemplateSimple (url opts) (LlamaApplyTemplateRequest request opts.model)
    case response of
      Nothing -> T.hPutStrLn stderr "Got no response from the server." >> exitFailure
      Just r -> T.putStrLn $ if stripThinking opts then snd $ T.breakOnEnd "</think>" r else r
  else
    case streaming opts of
      False -> do
        response <- llamaTemplatedRequest opts.url (LlamaApplyTemplateRequest request opts.model) (def { Llama.model = opts.model })
        case response of
          Nothing -> T.hPutStrLn stderr "Got no response from the server." >> exitFailure
          Just r -> T.putStrLn $ if stripThinking opts then snd $ T.breakOnEnd "</think>" r else r
      True -> do
        hSetBuffering stdout NoBuffering
        conduit <- llamaTemplatedStreamingRequest opts.url (LlamaApplyTemplateRequest request opts.model) (def { Llama.model = opts.model })
        runResourceT $ do
          list <- lazyConsume conduit
          liftIO $ mapM_ T.putStr $ (if stripThinking opts then dropSep "</think>" else id) $ map LS.content list
        T.putStrLn ""

dropSep :: Eq a => a -> [a] -> [a]
dropSep _ [] = []
dropSep a (x:xs) = if a == x then xs else dropSep a xs
