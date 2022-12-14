arr = "1 2 3"

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


for var1 in 1 2 3
do
   for var2 in 0 5
   do
      if [ $var1 -eq 2 -a $var2 -eq 0 ]
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