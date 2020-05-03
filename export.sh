#!/bin/sh
rm -rf ./dist
mkdir dist
runhaskell tables.hs >> ./dist/tables.csv
echo "exported into $PWD/dist/tables.csv"