# Zsh Script to start Tmux with specific config

# Main Session
SESSION=main

#                                         Window 1: System
tmux new-session -d -s $SESSION -n 'Sys' -c ~
tmux send-keys '. ~/.zshrc' C-m C-l
tmux split-window -t $SESSION:1 -v -p 70 -c ~/.config
tmux send-keys '. ~/.zshrc' C-m C-l

#                                         Window 2: Projects
#                                       W2> Pane 1: NeoVim
tmux new-window -t $SESSION:2 -n 'Projects' -c ~/share/Projects
tmux select-window -t $SESSION:2
tmux send-keys '. ~/.zshrc' C-m C-l

#                                       W2> Pane 2: smaller pane
tmux split-window -t $SESSION:2 -v -l 10 -c ~/share/Projects/Code/01_2fa_demo
tmux send-keys '. ~/.zshrc' C-m C-l

#                                         Window 3: Dev
tmux new-window -t $SESSION:3 -n 'Dev' -c ~/build
tmux send-keys '. ~/.zshrc' C-m C-l
#                                       W3> Pane 2: Two equal panes
tmux split-window -t $SESSION:3 -h -c ~/share/Projects/Code
tmux send-keys '. ~/.zshrc' C-m C-l

tmux attach -t $SESSION
