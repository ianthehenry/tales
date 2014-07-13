{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Text (Text, breakOn, stripStart, intercalate, pack, unpack, length)
import Text.Read (readMaybe)
import Data.Text.Lazy (fromStrict)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)

type User = String
data LogEntry = LogEntry User Text deriving (Show, Eq)
data BookOfTales = BookOfTales User [LogEntry]

holder :: BookOfTales -> User
holder (BookOfTales h _) = h

firstWord :: Text -> (Text, Text)
firstWord input = breakOn " " input & _2 %~ stripStart

helperText :: Text
helperText = "Observed commands are:\nread - list pages filled by tales past\nread page - read a tale\nwrite username tale - write a tale, and hand the book to another wanderer"

pshow :: Show a => a -> Text
pshow = pack . show

prettyPrintIndex :: Int -> LogEntry -> Text
prettyPrintIndex page (LogEntry user story) = mconcat [pshow page, ". ", pack user, " (", pshow (Data.Text.length story), " characters)"]

prettyPrintStory :: LogEntry -> Text
prettyPrintStory (LogEntry _ story) = story

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

writeBook :: BookOfTales -> User -> Text -> (BookOfTales, Text)
writeBook oldBook@(BookOfTales oldUser entries) newUser story
  | Data.Text.length story == 0 = (oldBook, mconcat ["The price of holding the Book of Tales is that ",
                                                     "you must write in its pages -- a hard responsibility, ",
                                                     "I know, but one that ", pack oldUser, " has placed upon your shoulders."])
  | oldUser == newUser = (oldBook, "I appreciate your enthusiasm, but the Book of Tales must change hands in order for the ink to take. It's part of... the magic.")
  | otherwise = let newBook  = BookOfTales newUser $ LogEntry oldUser story : entries
                    response = mconcat ["Your tale passes into legend; the Book passes safely to ", pack newUser, "'s hands."] in
                (newBook, response)

extractUser :: Text -> (User, Text)
extractUser input = firstWord input & _1 %~ unpack

dispatch :: BookOfTales -> (Text, Text) -> (BookOfTales, Text)
dispatch book ("", _)          = (book, mconcat ["Welcome to the book of tales! ", helperText])
dispatch book ("help", _)      = (book, helperText)
dispatch book ("read", input)  = (book, readBook book input)
dispatch book ("write", input) = (uncurry $ writeBook book) (extractUser input)
dispatch book ("steal", _)     = (book, "No; I took stealing out because it was unnecessarily complicated.")
dispatch book (cmd, _)         = (book, mconcat ["Unrecognized command \"", cmd, "\". ", helperText])

tales :: User -> Text -> BookOfTales -> (BookOfTales, Text)
tales requestor input book
  | requestor == holder book = dispatch book $ firstWord input
  | otherwise                = (book, "You do not hold the Book of Tales!")

incomingMessage :: MVar BookOfTales -> ActionM ()
incomingMessage bookVar = do
  user <- param "user"
  input <- param "text"
  response <- liftIO $ modifyMVar bookVar (return . tales user input)
  text . fromStrict $ response

main :: IO ()
main = do
  let book = BookOfTales "ian" [LogEntry "anonymous" "The Book of Tales is forged in the fires of Mount Haskell"]
  db <- newMVar book
  scotty 3000 $ do
    middleware logStdoutDev
    post "/" (incomingMessage db)
