{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Text (Text, breakOn, stripStart, intercalate, pack, unpack, length, strip)
import Text.Read (readMaybe)
import Data.Text.Lazy (fromStrict, toStrict)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Database.HDBC (prepare, execute, commit, fetchAllRows)
import Database.HDBC.SqlValue (SqlValue, fromSql, toSql)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Text.IO (readFile, writeFile)
import Prelude hiding (readFile, writeFile)
import qualified Network.Wreq as Wreq
import Network.Wreq (FormParam((:=)))
import Data.Aeson (encode)
import Data.Map (fromList, Map)
import Data.Text.Lazy.Encoding (decodeUtf8)

type User = String
type Token = Text
data LogEntry = LogEntry User Text UTCTime deriving (Show, Eq)
data BookOfTales = BookOfTales User [LogEntry] deriving (Show, Eq)
data NewBook = SameBook | NewEntry User Text

databasePath :: FilePath
databasePath = "book.db"

holderPath :: FilePath
holderPath = "holder"

holder :: BookOfTales -> User
holder (BookOfTales h _) = h

firstWord :: Text -> (Text, Text)
firstWord input = breakOn " " input & _2 %~ stripStart

helperText :: Text
helperText = "Valid commands:\nread - list pages filled by tales past\nread page - read a tale\nwrite username tale - write a tale, and hand the book to another wanderer"

pshow :: Show a => a -> Text
pshow = pack . show

prettyPrintIndex :: Int -> LogEntry -> Text
prettyPrintIndex page (LogEntry user story _) = mconcat [pshow page, ". ", pack user, " (", pshow (Data.Text.length story), " characters)"]

prettyPrintTime :: UTCTime -> Text
prettyPrintTime t = pack $ formatTime defaultTimeLocale "%b %e" t

prettyPrintStory :: LogEntry -> Text
prettyPrintStory (LogEntry username story timestamp) =
  mconcat ["By ", pack username, " on ", prettyPrintTime timestamp, "\n\n", story]

downFrom :: Int -> [Int]
downFrom x = [x, pred x..]

bookIndex :: BookOfTales -> Text
bookIndex (BookOfTales _ entries) = intercalate "\n" $ "All tales written in an age forgotten:" :
  zipWith prettyPrintIndex (downFrom pageCount) entries
  where pageCount = Prelude.length entries

readBook :: BookOfTales -> Text -> Text
readBook book "" = bookIndex book
readBook (BookOfTales _ entries) input = case readMaybe (unpack input) of
  Nothing -> "I don't understand that page number :/"
  Just n | n > 0 && n <= pageCount -> prettyPrintStory (entries !! (pageCount - n))
         | n == 0                  -> "Get outta here with that"
         | otherwise               -> "Invalid page"
  where pageCount = Prelude.length entries

writeBook :: BookOfTales -> User -> Text -> (NewBook, Text)
writeBook (BookOfTales oldUser _) newUser story
  | Data.Text.length story == 0 = (SameBook, mconcat ["The price of holding the Book of Tales is that ",
                                                      "you must write in its pages -- a hard responsibility, ",
                                                      "I know, but one that ", pack oldUser, " has placed upon your shoulders."])
  | oldUser == newUser          = (SameBook, "I appreciate your enthusiasm, but the Book of Tales must change hands in order for the ink to take. It's part of... the magic.")
  | otherwise                   = (NewEntry newUser story, mconcat ["Your tale passes into legend; the Book passes safely to ", pack newUser, "'s hands."])

extractUser :: Text -> (User, Text)
extractUser input = firstWord input & _1 %~ unpack

dispatch :: BookOfTales -> (Text, Text) -> (NewBook, Text)
dispatch _ ("", _)             = (SameBook, mconcat ["Welcome to the book of tales! ", helperText])
dispatch _ ("help", _)         = (SameBook, helperText)
dispatch book ("read", input)  = (SameBook, readBook book input)
dispatch book ("write", input) = (uncurry $ writeBook book) (extractUser input)
dispatch _ ("steal", _)        = (SameBook, "No; I took stealing out because it was unnecessarily complicated.")
dispatch _ (cmd, _)            = (SameBook, mconcat ["Unrecognized command \"", cmd, "\". ", helperText])

tales :: User -> Text -> BookOfTales -> (NewBook, Text)
tales requestor input book
  | requestor == holder book = dispatch book $ firstWord input
  | otherwise                = (SameBook, "You do not hold the Book of Tales!")

entryToSql :: LogEntry -> [SqlValue]
entryToSql (LogEntry username story timestamp) =
  [toSql username, toSql story, toSql timestamp]

insertLogEntry :: Connection -> LogEntry -> IO ()
insertLogEntry c newEntry = do
    insert <- prepare c "INSERT INTO entries VALUES (?, ?, ?);"
    execute insert (entryToSql newEntry)
    commit c

recordNewHolder :: User -> IO ()
recordNewHolder = writeFile holderPath . pack

notifyNewHolder :: User -> Token -> IO ()
notifyNewHolder user token = do
  let message = fromList [("channel" :: String, "@" ++ user),
                          ("username", "The Book of Tales"),
                          ("text", "You wake to find a strange book on your bedside table. On its cover, the letters \"/tales\" are scrawled..."),
                          ("icon_emoji", ":closed_book:")]

  let path = "https://trello.slack.com/services/hooks/incoming-webhook"
  let opts = Wreq.defaults & Wreq.param "token" .~ [token]
  Wreq.postWith opts path ["payload" := encode message]
  return ()

incomingMessage :: MVar BookOfTales -> Connection -> Token -> ActionM ()
incomingMessage bookVar conn token = do
  user <- param "user_name"
  input <- param "text"

  response <- liftIO $ modifyMVar bookVar (\book ->
    case tales user input book of
      (SameBook, response) -> return (book, response)
      (NewEntry newHolder story, response) -> do
        time <- liftIO getCurrentTime
        let newEntry = LogEntry oldHolder story time
        liftIO $ insertLogEntry conn newEntry
        liftIO $ recordNewHolder newHolder
        liftIO $ notifyNewHolder newHolder token
        return (BookOfTales newHolder (newEntry:oldEntries), response)
        where (BookOfTales oldHolder oldEntries) = book)

  text . fromStrict $ response

entryFromSql :: [SqlValue] -> LogEntry
entryFromSql [username, story, timestamp] = LogEntry (fromSql username) (fromSql story) (fromSql timestamp)

initialBook :: Connection -> IO BookOfTales
initialBook c = do
  select <- prepare c "SELECT username, story, timestamp FROM entries ORDER BY timestamp DESC;"
  execute select []
  result <- fetchAllRows select
  initialHolder <- fmap (unpack . strip) (readFile holderPath)
  return $ BookOfTales initialHolder (map entryFromSql result)

main :: IO ()
main = do
  conn <- connectSqlite3 databasePath
  book <- initialBook conn
  var <- newMVar book
  token <- readFile "token"
  scotty 3000 $ do
    middleware logStdoutDev
    post "/" (incomingMessage var conn token)
