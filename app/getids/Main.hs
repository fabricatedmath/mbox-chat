{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashTable.IO as H

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import qualified Data.Map as Map

import Control.Monad (forever)
import Control.Monad.IO.Class

import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Safe as P
import qualified Pipes.Text.IO as Text
import qualified Pipes.Prelude.Text as Text

import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import System.Environment (getArgs)
import System.IO

import Lib

type HashTable k v = H.CuckooHashTable k v

data Line = ChatLine Text | NotChatLine Text
  deriving Show

insert :: HashTable Text (Int,IntSet) -> Text -> Int -> IO ()
insert ht k v =
  do
    mimap <- H.lookup ht k
    case mimap of
      Just (n,imap) ->
        let imap' = IntSet.insert v imap
            n' = n+1
            tup = n' `seq` imap `seq` (n',imap')
        in tup `seq` H.insert ht k tup
      Nothing -> H.insert ht k (1, IntSet.singleton v)

process :: MonadIO m => HashTable Text (Int,IntSet) -> P.Consumer Text m r
process ht =
  let go =
        do
          _fromLine <- skipUntilFrom
          xgmThridLine <- P.await
          xGmailLabelsLine <- P.await
          case matchXGMTHRID xgmThridLine of
            Nothing -> error "No thrid"
            Just i ->
              do
                case matchXGmailLabelChat xGmailLabelsLine of
                  Just _ ->
                    do
                      email <- findEmail
                      liftIO $ insert ht email i
                  Nothing -> return ()
                go
  in go

process2 :: Monad m => IntSet -> P.Pipe Text (Maybe Text) m r
process2 set =
  let go lineType =
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
            Just i ->
              do
                case matchXGmailLabelChat xGmailLabelsLine of
                  Just _ ->
                    do
                      if IntSet.member i set
                        then yieldLines Just
                        else yieldLines $ const Nothing
                  Nothing -> yieldLines $ const Nothing
  in go errLine


writeFilePipe :: (MonadIO m, Text.MonadSafe m) => Text -> P.Consumer Text m r
writeFilePipe name =
  do
    handle <- liftIO $ openFile (Text.unpack name) WriteMode
    (forever $ P.await >>= liftIO . Text.hPutStrLn handle) `P.finally` (liftIO $ hClose handle >> putStrLn "Closed")

main :: IO ()
main =
  do
    (fileName:emails) <- getArgs
    ht <- H.new :: IO (HashTable Text (Int,IntSet))
    let
      producer = Text.readFileLn fileName
      pipe = forever $ P.await >>= P.yield


    Text.runSafeT $ P.runEffect $ producer >-> pipe >-> process ht
    l <- H.toList ht
    let maps = Map.unions $ map (\(x,(_,y)) -> Map.singleton x y) l
    mapM_ (\(x,(y,z)) -> putStrLn $ show x ++ "," ++ show y ++ "," ++ show (IntSet.size z)) l

    let
      emailText = map Text.pack emails
      emailSets = zip emailText $ map (fromJust . flip Map.lookup maps) emailText

    mapM_ (\(email,set) -> Text.runSafeT $ P.runEffect $ producer >-> process2 set >-> maybePipe >-> writeFilePipe email) emailSets
