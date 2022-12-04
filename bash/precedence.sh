#!/bin/sh

a=2
b=5

exp1=`a*(b-a)`
exp2=(a*`b-a`)

echo "First expression yields $exp1"
echo "Second expression yields $exp2"
