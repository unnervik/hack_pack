module TestSpecParser
    ( Protocol(..)
    , Action(..)
    , p_action
    ) where

import ApplicativeParsec

data Protocol = HTTP | FTP | SSH

data Action = Action {
      protocol :: Protocol
    , range    :: [Int]
    } deriving (Eq, Show)

p_action :: CharParser () Action
p_action = undefined -- many1 char <* many1 ""