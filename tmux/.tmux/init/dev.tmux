# Zsh Script to start Tmux with specific config

# Development Session
SESSION=dev

#                                         Window 1: Vifm
tmux new-session -d -c ~ -s $SESSION -n 'vifm'
#tmux split-window -t $SESSION:1 -v -p 70 'colorscript -r' 

#                                         Window 2: nvim
#                                       W2> Pane 1: NeoVim
#tmux new-window -t $SESSION:2 -n 'nvim' 
#tmux select-window -t $SESSION:2
#tmux send-keys 'nvim .' C-m
#                                       W2> Pane 2: smaller pane
#tmux split-window -t $SESSION:2 -v -l 10 'colorscript -r' 

#                                         Window 3: Python
#tmux new-window -t $SESSION:3 -n 'python'
#                                       W3> Pane 2: Two equal panes
#tmux split-window -t $SESSION:3 -h

tmux attach -t $SESSION
