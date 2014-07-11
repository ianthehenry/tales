{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Text (Text, breakOn, stripStart)
import Data.Text.Lazy (fromStrict)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Control.Lens
import Data.Monoid (mconcat)
import Data.Functor ((<$>))

type User = String
data LogEntry = LogEntry {story :: Text, user :: User} deriving (Show, Eq)
type BookOfTales = [LogEntry]

currentUser :: BookOfTales -> User
currentUser (x:_) = user x

-- BEHAVE IN THIE FOLLOWING MANNER:
-- /tales
--   read [page] (if page is omitted, it returns the index. page is just the index of the entry)
--   write username Text...

token = "MEv3EuIShbsE73cIMRd3C9d1"

firstWord :: Text -> (Text, Text)
firstWord input = breakOn " " input & _2 %~ stripStart

dispatch :: (Text, Text) -> Text
dispatch ("", _) = "Welcome to the book of tales! Possible commands are:\nread - list pages filled by tales past\nread page - read a tale\nwrite username tale - write a tale, and hand the book to another wanderer"
dispatch ("help", _) = dispatch ("", "")
dispatch ("read", "") = "no entries yet"
dispatch ("read", _) = "reading a specific page ..."
dispatch ("write", _) = "writing"
dispatch ("steal", _) = "you have failed to steal, and have lost 1 xp"
dispatch (cmd, _) = mconcat ["unrecognized command ", cmd, "!"]

tales :: Text -> Text
tales = dispatch . firstWord

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  post "/" $ (text . fromStrict) =<<
    tales <$> (param "text")

