module Main where

import LogAnalysis
import Log

main :: IO ()
main = do
    logs <- testParse parse 5523 "error.log"
    let messageTree = build logs
    print messageTree
    print (whatWentWrong messageTree)