# Zsh Script to start Tmux with specific config

# Main Session
SESSION=main

#                                         Window 1: System
tmux new-session -d -s $SESSION -n 'Sys' -c ~
tmux split-window -t $SESSION:1 -v -p 70 -c ~/.config

#                                         Window 2: Projects
#                                       W2> Pane 1: NeoVim
tmux new-window -t $SESSION:2 -n 'Projects' -c ~/share/Projects
tmux select-window -t $SESSION:2

#                                       W2> Pane 2: smaller pane
tmux split-window -t $SESSION:2 -v -l 10 -c ~/share/Projects/Code/01_2fa_demo

#                                         Window 3: Dev
tmux new-window -t $SESSION:3 -n 'Dev' -c ~/build
#                                       W3> Pane 2: Two equal panes
tmux split-window -t $SESSION:3 -h -c ~/share/Projects/Code

tmux attach -t $SESSION
