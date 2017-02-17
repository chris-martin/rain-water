module Main where

import Rain

import Protolude

main :: IO ()
main = print (collectWater [2,5,1,3,1,2,1,7,7,6])
