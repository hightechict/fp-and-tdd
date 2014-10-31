tmux new-session -s tdd -n editor -d
tmux send-keys -t tdd './select-editor.sh' C-m
tmux split-window -v -t tdd -p 30
tmux select-pane -t tdd -U
tmux attach -t tdd
