# clear
alias c="clear"

# clean directory
alias cleanup="/bin/rm -f *# *~"

# Directories
alias home="cd ~"
alias root="cd /"

# ls/tree
alias ll="ls -lh"
alias la="ls -Alh"
alias trea="tree -a"

# safe rm, mv, and cp
alias rm="rm -i"
alias mv="mv -i"
alias cp="cp -i"

# color aliases (may already exist, but ¯\_(ツ)_/¯)
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# IPython
if [[ -z $(uname -s | grep -E "(MINGW|MSYS)") ]]; then
    # Windows
    alias ipy=ipython.exe
else
    alias ipy=ipython
fi

# CPU temp
cputemp() {
    paste <(cat /sys/class/thermal/thermal_zone*/type) <(cat /sys/class/thermal/thermal_zone*/temp) | column -s $'\t' -t | sed 's/\(.\)..$/.\1°C/'
}

# Poetry list outdated packages
poetry-outdated() {
    poetry show --outdated | grep --file=<(poetry show --tree | grep '^\w' | cut -d' ' -f1)
}

#
# git
#
git() {
    cmd=$1
    shift

    case $cmd in
    a)
        command git add $@
        ;;
    aa)
        command git add -a $@
        ;;
    c)
        command git commit $@
        ;;
    ca)
        command git commit -a $@
        ;;
    cam)
        command git commit -a -m $@
        ;;
    cm)
        command git commit -m $@
        ;;
    p)
        command git push $@
        ;;
    pl)
        command git pull $@
        ;;
    s)
        command git status $@
        ;;
    unadd)
        shift
        command git restore --staged $@
        ;;
    *)
        command git $cmd $@
        ;;
    esac
}

#
# apt
#
apt() {
    ## check if is sudoer
    if ! $(sudo -l &>/dev/null); then
        echo 'ERROR: root privileges are needed to run apt!'
        return 1
    fi

    cmd=$1
    shift

    case $cmd in
    ac)
        command sudo apt autoclean $@
        ;;
    c)
        command sudo apt clean $@
        ;;
    i)
        command sudo apt install $@
        ;;
    rm)
        command sudo apt remove $@
        ;;
    up)
        command sudo apt update $@
        ;;
    upg)
        command sudo apt upgrade $@
        ;;
    *)
        command apt $cmd $@
        ;;
    esac
}

#
# tar
#
untar() {
    if [[ $# -eq 1 ]]; then
        # Detect from extension
        filename=$(basename $1)
        archive_type="${filename##*.}"
    elif [[ $# -eq 2 ]]; then
        # archive type given
        archive_type="$1"
        shift
    else
        echo "ERROR: Invlalid arguments"
        echo "Usage: untar <archive type> [directory]"
        return 1
    fi

    case $archive_type in
    gz | tgz)
        decompress_arg="z"
        ;;
    xz | txz)
        decompress_arg="J"
        ;;
    bz2 | tbz2)
        decompress_arg="j"
        ;;
    *)
        echo "ERROR: Must specify valid tar format or file."
        return 1
        ;;
    esac

    command tar -xv$decompress_arg -f $1
}

mktar() {
    if [[ $# -eq 2 ]]; then
        # archive type given
        archive_type="$1"
        shift
    else
        echo "ERROR: Invlalid arguments"
        echo "Usage: mktar [archive type] [directory]"
        return 1
    fi

    case $archive_type in
    gz | tgz)
        compress_arg="z"
        ext="gz"
        ;;
    xz | txz)
        compress_arg="J"
        ext="xz"
        ;;
    bz2 | tbz2)
        compress_arg="j"
        ext="bz2"
        ;;
    *)
        echo "ERROR: Must specify valid tar format or file."
        return 1
        ;;
    esac

    dir_name=$(basename $1)

    command tar -cv$compress_arg -f "$dir_name.tar.$ext" $dir_name
}
