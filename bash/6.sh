#!/bin/bash

for var1 in 1 2 3 4 5
do
   echo "Hello I am #$var1"
done

for varx in "jason" "kyg" "calvin" "Emma"
do
   localvar=`"hello " += $varx`
   echo "$localvar"
done

for var1 in 1 2 3
do 
   for var2 in 3 2 1
   do 
      echo "row $var1 col $var2"
   done
done