{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE BlockArguments #-}

module Main where

import ACE

main :: IO () 
main = withACE $ do
    env <- mkEnv OP2 XH035
    randomSizing env >>= evaluate env >>= print

    envs <- mkEnvs OP2 XH035 5
    randomSizing' envs >>= evaluate' envs >>= print
    pure ()
