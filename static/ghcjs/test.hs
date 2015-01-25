{-# LANGUAGE OverloadedStrings #-}

module Main where

import JavaScript.JQuery

main = do
  putStrLn "Hello, world!"
  r <- ajax3 "test1" ("string" :: String)
  putStrLn $ "test1: r = " ++ show r
