{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad (forever)

import Data.Maybe (isJust)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import qualified Pipes as P

days :: [Text]
days = ["Mon","Tue","Wed","Thu","Fri","Sat","Sun"]

matchFromLine :: Text -> Maybe UTCTime
matchFromLine = matchFromLine' . Text.splitOn " "
  where
    matchFromLine' ("From":addr:xs)
      | isJust (Text.find (=='@') addr) =
          Just $ parseTimeOrError True defaultTimeLocale "%c" $ Text.unpack $ Text.intercalate " " xs
      | otherwise = Nothing
    matchFromLine' _ = Nothing

matchXGMTHRID :: Text -> Maybe Int
matchXGMTHRID line =
  case Text.stripPrefix "X-GM-THRID: " line of
    Nothing -> Nothing
    Just t ->
      case Text.decimal t of
        Left err -> error err
        Right (i,_) -> Just i

matchXGmailLabelChat :: Text -> Maybe Text
matchXGmailLabelChat = Text.stripPrefix "X-Gmail-Labels: Chat"

getFromEmail :: Text -> Maybe Text
getFromEmail line =
  case Text.stripPrefix "From: " line of
    Just x ->
      let (f,s) = Text.break (=='<') x
      in case Text.null s of
           True -> Just $ Text.init f
           False -> Just $ Text.init $ Text.tail $ Text.init $ s
    Nothing -> Nothing

findEmail :: Monad m => P.Consumer Text m Text
findEmail =
  let
    go =
      do
        line <- P.await
        case matchFromLine line of
          Just _ -> error "err"
          Nothing ->
            case getFromEmail line of
              Just email -> return email
              Nothing -> go
  in go

skipUntilFrom :: Monad m => P.Consumer Text m ()
skipUntilFrom =
  let go =
        do
          line <- P.await
          case matchFromLine line of
            Just _ -> return ()
            Nothing -> go
  in go

data Line = ChatLine Text | NotChatLine Text
          deriving Show

errLine :: Text -> a
errLine = error . Text.unpack

maybePipe :: Monad m => P.Pipe (Maybe a) a m r
maybePipe =
  forever $ do
    m <- P.await
    case m of
      Just x -> P.yield x
      Nothing -> return ()

passUntilFrom :: Monad m => (Text -> a) -> P.Pipe Text a m Text
passUntilFrom f =
  let go =
        do
          line <- P.await
          case matchFromLine line of
            Just _ -> return line
            Nothing -> P.yield (f line) >> go
  in go
