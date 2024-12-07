{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Parse das mensagens

parseType :: [String] -> LogMessage
parseType [] = Unknown ""
parseType ("I":xs) = parseTimeStamp xs Info
parseType ("W":xs) = parseTimeStamp xs Warning
parseType ("E":severity:xs) = parseTimeStamp xs (Error (read severity))
parseType xs = Unknown (unwords xs)

parseTimeStamp :: [String] -> MessageType -> LogMessage
parseTimeStamp(time: xs) msgType = LogMessage msgType (read time) (unwords xs)
parseTimeStamp _ _ = Unknown "Malformed log"

parseMessage :: String -> LogMessage
parseMessage xs = parseType (words xs)

parse :: String -> [LogMessage]
parse logData = map parseMessage (lines logData)

-- Árvore

insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ ts1 _) (Node left rootMsg@(LogMessage _ ts2 _) right)
 | ts1 < ts2  = Node (insert logMsg left) rootMsg right
 | otherwise  = Node left rootMsg (insert logMsg right)
insert _ tree = tree


build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: MessageTree -> [String]
whatWentWrong logMsgs = 
  let orderedLogMsgs = inOrder logMsgs
  in [message | LogMessage (Error severity) _ message <- orderedLogMsgs, severity >= 50]