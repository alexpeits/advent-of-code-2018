#!/bin/bash

day="$(printf %02d $1)"
dir="src/Advent"
f="$dir/Day$day.hs"

echo "module Advent.Day$day where" >> $f
echo "" >> $f
echo "import Advent.Util" >> $f
echo "" >> $f
echo "day :: Int" >> $f
echo "day = $1" >> $f
echo "" >> $f
echo "main :: IO ()" >> $f
echo "main = undefined" >> $f
