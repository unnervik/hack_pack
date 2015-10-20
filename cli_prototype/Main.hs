module Main where

import Data.List
import TestSpecParser

help :: String
help = "hack_pack Command Line Interface. Options:\n" ++
       "    exit\n" ++
       "    load - filename.testspec\n" ++
       "    help - show this message\n"

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy delimiter = foldr f [[]]
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

main :: IO ()
main = do
    putStr help
    line <- getLine
    case splitBy ' ' line of
        ("exit" : _) ->
            return ()
        ["load", filename] -> do
            putStrLn $ "Load testspec " ++ filename ++ " ..."
            main
        ["help"] -> do
            putStr help
            main
