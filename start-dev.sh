cd
tmux new-session -s tdd -n editor -d
tmux send-keys -t tdd '~/.bin/select-editor.sh' C-m
tmux split-window -v -t tdd -p 30
tmux send-keys -t tdd 'watchr ~/.bin/run-tests.watchr' C-m
tmux new-window -t tdd:2 -n chicken 'csi' 
tmux split-window -v -t tdd:2
tmux select-pane -t tdd:2.1
tmux select-window -t tdd:1
tmux select-pane -t tdd:1.1
tmux attach -t tdd
