module TestSpecParser
    ( Protocol(..)
    , Action(..)
    , p_action
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec
-- import ApplicativeParsec

data Protocol = HTTP | FTP | SSH
    deriving (Eq, Show)

data Action = Action {
      protocol :: Protocol
    , range    :: [Int]
    } deriving (Eq, Show)

hmm :: Action
hmm = case parse p_action "unknown" "wdijidjawdiwj" of
          Left x  -> error "No parse"
          Right y -> y

p_action :: Parser Action
p_action = do
    x <- many1 alphaNum
    return $ Action HTTP [1..3]

p_applicative :: Parser (String, String)
p_applicative = (,) <$> many1 letter <*> (spaces *> many1 digit)
