module LogAnalysis where

data LogItem = LogItem String String String
  deriving (Show)

processLog :: String -> LogItem
processLog logItem =
  let items = words logItem
      logType = (!!) items 0
      timeStamp = (!!) items 1
      content = (!!) items 2
  in LogItem logType timeStamp content

main = do
  file <- readFile "sample.log"
  let logs = lines file
  print logs
