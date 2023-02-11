(specifications->manifest
 (list "file"                   ; Guessing Filetypes
       "git"
       "the-silver-searcher"    ; ag: Better and Faster grep/awk
       "ripgrep"                ; rg: Better than ag?

       ;; Spell Checking
       "aspell"
       "aspell-dict-en"
       "aspell-dict-de"
       "wordnet"

       ;; Programming
       "node"           ; npm: To install lsp server
       "icedtea"        ; jre: Java Runtime Environment (alternative: openjdk)
       "python"         ; What else?
       "python-scipy"   ; Includes numpy
       "python-pandas"  ; DataFrames
       "python-seaborn" ; Includes matplotlib
       "python-sphinx"
       "python-numpydoc"
       "python-pynvim"

       ;; Visualization Tools
       "plantuml"       ; UML Diagrams
       "graphviz"       ; Convert Graphs

       ;; GUI Tools
       "xdot"           ; Display Graphs
       "xsel"           ; Manipulate Selections

       ;; Converting Files
       "pandoc"         ; Swiss-Army-Knife
       ;; "texlive"        ; LaTeX
       ;; "texlive-latex-moderncv"
       ;; "texlive-latex-fontawesome"

       ;; GUI Fonts
       "font-nerd-fonts"
       "font-font-awesome"
       "font-juliamono"
       ;; "font-jetbrains-mono"
       ;; "font-overpass"

       ;; Encryption
       "gnupg"          ; GnuPG
       "sshfs"          ; SSH file systems
       "pinentry"))     ; GUI Password Entry
