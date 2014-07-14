# The Book of Tales

A "Do Something With Haskell" experiment. Converts outgoing webhooks from Slack into whimsy.

## How to make it work

    $ sqlite3 book.db ".read schema.sql"
    $ echo "yourname" > holder
    $ echo "token" > token
    $ cabal run

## Not done yet

- username validation when handing off the book (whitelist?)
- notify users when they receive the book
- timeouts if you horde the book for too long
