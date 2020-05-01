#!/bin/sh
rm -rf ./dist
mkdir dist
runhaskell tables.hs >> ./dist/TABLES