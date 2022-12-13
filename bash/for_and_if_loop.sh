#!/bin/sh

arr="1 2 3"

for var1 in true true false
do
   echo "hi $var1"
done

for var1 in 1 2 3 
do
   echo "hi $var1"
done

for var1 in "1" "2" "3"
do
   echo "hi $var1"
done

# Ansewer is 
# hi 1
# hi 2
# hi 3


for var1 in "1" 2 true
do
   echo "hi $var1"
done

# Ansewer is 
# hi 1
# hi 2
# hi true