#!/bin/bash


DIR=$1
DIRLIST=`find $DIR| grep ".rb"`

#echo $DIRLIST

for FILE in $DIRLIST 
do
./joern --import $FILE 1>/dev/null 2>&1

if [ $? == 1 ]
then
echo "FAILED to import "$FILE" !!!!!!!!!"
else
echo "Successfully imported "$FILE
fi
done
