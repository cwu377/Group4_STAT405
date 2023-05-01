#!/bin/bash

for file in $(ls -1 /home/groups/STAT_DSCP/group1_Spring2023/*.tsv); do
	echo $file
	head -n 1 $file > $file.short.tsv
	tail -n +2 $file | shuf -n 500000 $file >> $file.short.tsv
done
