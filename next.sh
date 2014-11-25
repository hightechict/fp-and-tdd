#! /bin/env bash

old_IFS=$IFS      # save the field separator           
IFS=$'\n'     # new field separator, the end of line           
for line in $(cat slides.txt)          
do          
  read -p ""
  #echo "sending $line to presenter:1.1"
  tmux send-keys -t presenter:1.1 $line
  read -p ""
  tmux  send-keys -t presenter:1.1 C-m
done          
IFS=$old_IFS     # restore default field separator 

