#!/bin/sh 

cd $1
ghc --make SlimTest.hs
./SlimTest
