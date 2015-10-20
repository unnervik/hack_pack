module Main where

import Data.List
import System.IO
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

interpret :: [Action] -> IO ()
interpret [] = return ()
interpret (a:as) = do
    putStrLn $ "Performing action "   ++ name a
    putStrLn $ "    using hosts: "    ++ show (hosts a)
    putStrLn $ "    at ports: "       ++ show [fst (ports a) .. snd (ports a)]
    putStrLn $ "    with protocols: " ++ show (protocol a)
    interpret as

main :: IO ()
main = do
    putStr help
    line <- getLine
    case splitBy ' ' line of
        ("exit" : _) ->
            return ()
        ["load", filename] -> do
            putStrLn $ "Load testspec " ++ filename ++ " ..."
            file <- readFile filename
            case parseSpec file of
                Left error ->
                    putStrLn $ "Parse error: " ++ (show error)
                Right actionList ->
                    interpret actionList
            main
        ["help"] -> do
            putStr help
            main
