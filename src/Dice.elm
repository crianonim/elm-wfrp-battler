module Dice exposing (..)
import Random

rollGenerator : Int -> Int -> Random.Generator (List Int)
rollGenerator count faces = Random.list count (Random.int 1 faces) 

d100Generator = Random.int 1 100

rollResult : List Int -> Int
rollResult dice = List.foldl (+) 0 dice