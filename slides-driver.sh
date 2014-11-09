#! /bin/env bash
tmux new-session -s presenter -n slides -d
tmux send-keys -t presenter 'csi -q -w' C-m
tmux split-window -v -t presenter -l 1
tmux new-window -t presenter -n start-fizzbuzz 
tmux send-keys -t presenter 'vim fizzbuzz-start.scm' C-m
tmux split-window -v -t presenter
tmux send-keys -t presenter 'watchr run-tests.watchr' C-m
tmux select-pane -t presenter:2.1
tmux new-window -t presenter -n finished-fizzbuzz 
tmux send-keys -t presenter 'vim fizzbuzz.scm' C-m
tmux new-window -t presenter -n start-stack 
tmux send-keys -t presenter 'vim stack-calculator-start.scm' C-m
tmux new-window -t presenter -n finished-stack 
tmux send-keys -t presenter 'vim stack-calculator.scm' C-m
tmux select-window -t presenter:1
tmux select-pane -t presenter:1.2
tmux send-keys -t presenter:1.2 './next.sh' C-m
tmux attach -t presenter
