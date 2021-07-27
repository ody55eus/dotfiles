# Function to source files if they exist
function zsh_add_file() {
    [ -f "$ZDOTDIR/$1" ] && source "$ZDOTDIR/$1"
}

function zsh_add_plugin() {
    PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
    if [ -d "$ZDOTDIR/plugins/$PLUGIN_NAME" ]; then 
        # For plugins
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh" || \
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"
    else
        git clone "https://github.com/$1.git" "$ZDOTDIR/plugins/$PLUGIN_NAME"
    fi

}

function zsh_add_completion() {
    PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
    if [ -d "$ZDOTDIR/plugins/$PLUGIN_NAME" ]; then 
        # For completions
        completion_file_list=$(ls -R $ZDOTDIR/plugins/$PLUGIN_NAME/**/_*)
        completion_file_path=${completion_file_list:1}
        fpath+="$(dirname "${completion_file_path}")"
        zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin.zsh"
    else
        git clone "https://github.com/$1.git" "$ZDOTDIR/plugins/$PLUGIN_NAME"
        fpath+=$(ls $ZDOTDIR/plugins/$PLUGIN_NAME/_*)
        [ -f $ZDOTDIR/.zccompdump ] && $ZDOTDIR/.zccompdump
    fi
    completion_file="$(basename "${completion_file_path}")"
    if [ "$2" = true ] && compinit "${completion_file:1}"
}

function zsh_load_config() {   
    CONF_DIR="$ZDOTDIR/$1"
    # Load all of the config files in ~/oh-my-zsh that end in .zsh
    # TIP: Add files you don't want in git to .gitignore
    for config_file ($CONF_DIR/*.zsh); do
        custom_config_file="${CONF_DIR}/${config_file:t}"
        [ -f "${custom_config_file}" ] && config_file=${custom_config_file}
        source $config_file
    done
}

function load_theme() {
    # Load the theme
    if [ ! "$ZSH_THEME" = ""  ]; then
      if [ -f "$ZSH_CUSTOM/$ZSH_THEME.zsh-theme" ]; then
        source "$ZSH_CUSTOM/$ZSH_THEME.zsh-theme"
      elif [ -f "$ZSH_CUSTOM/themes/$ZSH_THEME.zsh-theme" ]; then
        source "$ZSH_CUSTOM/themes/$ZSH_THEME.zsh-theme"
      else
        source "$ZSH/themes/$ZSH_THEME.zsh-theme"
      fi
    fi
}
