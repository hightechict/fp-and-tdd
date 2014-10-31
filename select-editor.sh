#! /usr/bin/env bash
exec 3>&1
choice=`dialog --backtitle "Functional Programming and Test Driven Development" \
--title "Select an editor" \
--menu "Please select the editor you would like to use during this workshop:" \
15 87 7 \
vim "VI improved, the improved version of the infamous modal vi editor." \
emacs "The operating system disguised as an editor." \
diakonos "A console text editor with keybindings similar to GUI editors." \
mcedit "The editor of midnight commander." \
mg "A small and fast emacs like editor." \
nano "A free clone of the pico editor, simple and easy to use." \
pico "The editor used in the pine mail program." 2>&1 1>&3`
retval=$?
exec 3>&-
clear
if [ $retval -ne 0 ]; then
	echo "Please open the file fizzbuzz.scm in your favorite editor."
fi
file=fizzbuzz.scm
exec $choice $file
