#! /bin/env bash

old_IFS=$IFS      # save the field separator           
IFS=$'\n'     # new field separator, the end of line           
for line in $(cat slides.txt)          
do          
  #echo "sending $line to test:1.1"
  tmux send-keys -t test:1.1 $line
  read -p ""
  tmux  send-keys -t test:1.1 C-m
  read -p ""
done          
IFS=$old_IFS     # restore default field separator 
