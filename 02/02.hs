{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

----------
-- ex1

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parseWords :: [String] -> LogMessage
parseWords wrds = case wrds of
    ("I":tm:msg)     -> LogMessage Info               (read tm) (unwords msg)
    ("W":tm:msg)     -> LogMessage Warning            (read tm) (unwords msg)
    ("E":lvl:tm:msg) -> LogMessage (Error $ read lvl) (read tm) (unwords msg)
    _                -> Unknown $ unwords wrds

parse :: String -> [LogMessage]
parse = map parseMessage . lines

----------
-- ex2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert _ (Node _ (Unknown _) _) = error "Malformed tree: no Unknowns allowed"
insert new Leaf = Node Leaf new Leaf
insert a@(LogMessage _ ats _) (Node left b@(LogMessage _ bts _) right)
    | ats > bts = Node left b (insert a right)
    | otherwise = Node (insert a left) b right

----------
-- ex3

build :: [LogMessage] -> MessageTree
build []  = Leaf
build (x:xs) = x `insert` build xs

----------
-- ex4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

sort :: [LogMessage] -> [LogMessage]
sort = inOrder . build

----------
-- ex5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . sort . filter severeError

severeError :: LogMessage -> Bool
severeError (LogMessage (Error lvl) _ _)
  | lvl > 50  = True
severeError _ = False

message :: LogMessage -> String
message (LogMessage _ _ msg) = msg
message (Unknown msg)        = msg

----------
-- ex6

sortedLog :: IO [LogMessage]
sortedLog = sort . parse <$> readFile "error.log"

writeFullLog :: IO ()
writeFullLog = unlines . fmap show <$> sortedLog >>= writeFile "sorted.log"
