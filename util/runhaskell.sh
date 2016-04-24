#!/bin/bash
cd ..
cabal-1.22 build
if ($?); then 
echo fail
else
./dist/build/GeoCurrBack/GeoCurrBack
echo "wow, we did it!"
fi