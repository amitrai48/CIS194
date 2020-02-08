{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

data FailableInt = FailedInt 
  | SuccessInt Int
  deriving Show

type MessageTypeAndRest = (MessageType, [String])

data FailableMessageTypeAndRest = FailedMessageTypeAndRest
  | SuccessMessageTypeAndRest MessageTypeAndRest
  deriving Show

toInt :: String -> FailableInt
toInt s = case reads s :: [(Int, String)] of
  [(a, "")] -> SuccessInt a
  _         -> FailedInt

splitOnMessageType :: String -> FailableMessageTypeAndRest
splitOnMessageType [] = FailedMessageTypeAndRest
splitOnMessageType s = case words s of
  (['I']:rest) -> SuccessMessageTypeAndRest (Info, rest)
  (['W']:rest) -> SuccessMessageTypeAndRest (Warning, rest)
  (['E']: severity: rest) -> case toInt severity of
    (SuccessInt level) -> SuccessMessageTypeAndRest (Error level, rest)
    _ -> FailedMessageTypeAndRest
  _ -> FailedMessageTypeAndRest

parseMessage :: String -> LogMessage
parseMessage s = case splitOnMessageType s of
  SuccessMessageTypeAndRest (messageType, rest) -> case rest of
    (timestamp: message) -> case toInt timestamp of
      SuccessInt time -> LogMessage messageType time (unwords message)
      FailedInt -> Unknown s
    _ -> Unknown s 
  FailedMessageTypeAndRest -> Unknown s


parseMessages :: [String] -> [LogMessage]
parseMessages [] = []
parseMessages (first:rest) = parseMessage first : parseMessages rest

parse :: String -> [LogMessage]
parse s = parseMessages (lines s)

createRoot :: LogMessage -> MessageTree
createRoot logMessage = Node Leaf logMessage Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) tree = tree
insert logMessage Leaf = createRoot logMessage
insert logMessage@(LogMessage _ newLogTimestamp _) (Node leftTree tlog@(LogMessage _ timestamp _) rightTree) = case newLogTimestamp < timestamp of
  True -> Node (insert logMessage leftTree) tlog rightTree
  False -> Node leftTree tlog (insert logMessage rightTree)
insert _ tree = tree

buildMessageTree :: MessageTree -> [LogMessage] -> MessageTree
buildMessageTree tree [] = tree
buildMessageTree tree (first: rest) = insert first (buildMessageTree tree rest)

build :: [LogMessage] -> MessageTree
build = buildMessageTree Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++ [logMessage] ++ inOrder right

isSevere :: LogMessage -> Int -> Bool
isSevere (LogMessage (Error severity) _ _) minSeverity = severity >= minSeverity
isSevere _ _ = False
 
filterLogMessageBySeverity :: [LogMessage] -> Int -> [LogMessage]
filterLogMessageBySeverity [] _= []
filterLogMessageBySeverity (logMessage:rest) minSeverity = case isSevere logMessage minSeverity of
  True -> logMessage : filterLogMessageBySeverity rest minSeverity
  False -> filterLogMessageBySeverity rest minSeverity

stringifyLogMessages :: [LogMessage] -> [String]
stringifyLogMessages [] = []
stringifyLogMessages (LogMessage _ _ message : rest) = message : stringifyLogMessages rest
stringifyLogMessages (Unknown message : rest) = message : stringifyLogMessages rest 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = stringifyLogMessages (inOrder (build (filterLogMessageBySeverity logs 50)))
