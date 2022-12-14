#!/bin/bash

# Iterates the exterior array until the elemetn equals 2, 
# and the interior array until equals 0, then break 
for var1 in 1 2
do
   for var2 in 0 5
   do
      if [ [$var1 -eq 2] -a [$var2 -eq 0] ]
      then
         break
      else
         echo "$var1 $var2"
      fi
   done
done

# Answer is: 
# 1 0
# 1 5