{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class

import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Safe as P
import qualified Pipes.Text.IO as Text
import qualified Pipes.Prelude.Text as Text

import Data.Text (Text)
import qualified Data.Text.IO as Text

import System.IO

import System.Environment (getArgs)

import Lib

matchEmail :: MonadIO m => P.Pipe Text Line m r
matchEmail = process

process :: MonadIO m => P.Pipe Text Line m r
process =
  let
    go lineType =
      do
        fromLine <- passUntilFrom lineType
        xgmThridLine <- P.await
        xGmailLabelsLine <- P.await
        let
          yieldLines f =
            do
              P.yield $ f fromLine
              P.yield $ f xgmThridLine
              P.yield $ f xGmailLabelsLine
              go f
        case matchXGMTHRID xgmThridLine of
          Nothing -> error "No thrid"
          Just _ ->
            case matchXGmailLabelChat xGmailLabelsLine of
              Just _ -> yieldLines ChatLine
              Nothing -> yieldLines NotChatLine
  in go errLine

writeFiles :: (MonadIO m, Text.MonadSafe m) => P.Consumer Line m r
writeFiles =
  do
    chatHandle <- liftIO $ openFile "Chat.mbox" WriteMode
    notChatHandle <- liftIO $ openFile "NotChat.mbox" WriteMode
    (
      forever $ do
        line <- P.await
        liftIO $ case line of
                   ChatLine text -> Text.hPutStrLn chatHandle text
                   NotChatLine text -> Text.hPutStrLn notChatHandle text
      ) `P.finally` (liftIO $ do
                      hClose chatHandle
                      hClose notChatHandle
                      putStrLn "Closed"
                  )

main :: IO ()
main =
  do
    [fileName] <- getArgs
    let
      producer = Text.readFileLn fileName
    Text.runSafeT $ P.runEffect $ producer >-> matchEmail >-> writeFiles
