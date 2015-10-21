module TestSpecParser
    ( Protocol(..)
    , Action(..)
    , parseSpec
    ) where

import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec

data Protocol = HTTP1 | HTTP2 | FTP | SSH
    deriving (Eq, Read, Show)

data Action = Action {
      name     :: String
    , protocol :: [Protocol]
    , hosts    :: (String, String)
    , ports    :: (Int, Int)
    } deriving (Eq, Show)

parseSpec :: String -> Either ParseError [Action]
parseSpec str = parse p_spec "unknown" str

eol :: Parser Char
eol = char '\n'

p_comment :: Parser ()
p_comment = do
    char '#'
    manyTill anyChar eol
    --many1 anyChar `endBy` (char '#')
    return ()

p_name :: Parser String
p_name = do
    char '-'
    name <- many1 alphaNum
    char '-'
    return name

p_array :: Parser [String]
p_array = do
    char '['
    arr <- (many alphaNum) `sepBy` char ','
    char ']'
    return arr

p_range :: Parser (String, String)
p_range = liftA2 (,) (char '[' *> ra <* string "->")
                     (ra <* char ']')
    where ra = many1 $ try alphaNum <|> char '.'

p_action :: Parser Action
p_action = do
    many p_comment
    name <- p_name
    spaces
    string "protocol:"
    spaces
    ptcls <- p_array
    spaces
    string "address:"
    spaces
    (hostFrom, hostTo) <- p_range
    spaces
    string "ports:"
    spaces
    (portFrom, portTo) <- p_range
    spaces
    return $ Action name
                    (map read ptcls)
                    (hostFrom, hostTo)
                    (read portFrom, read portTo)

p_spec :: Parser [Action]
p_spec = many p_action -- <* eof

test = parseSpec $ "#iojkijij\n" ++
                   "#kkokok\n" ++
                   "-name-\n" ++
                   "protocol: [HTTP1,HTTP2]\n" ++
                   "address: [192.168.0.1->192.168.0.10]\n" ++
                   "ports: [3->4]\n\n" ++
                   "#iojkijij\n" ++
                   "#kkokok\n" ++
                   "-name-\n" ++
                   "protocol: [HTTP1,HTTP2]\n" ++
                   "address: [192.168.0.1->192.168.0.10]\n" ++
                   "ports: [3->4]"
