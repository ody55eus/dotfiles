new -s TestSession -n TestWindow vim
neww -n foo
splitw -v -p 50 -t 0 bar
neww -n bar
splitw -h -p 80 -t 0 foo/bar
selectw -t 1
selectp -t 0