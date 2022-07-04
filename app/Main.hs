{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE BlockArguments #-}

module Main where

import ACE

main :: IO () 
main = withACE $ do
    env <- mkEnv OP2 XH035
    randomSizing env >>= simulate env >>= print
