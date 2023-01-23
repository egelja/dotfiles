#
# CD
#
# https://mhoffman.github.io/2015/05/21/how-to-navigate-directories-with-the-shell.html
function cd() {
    if [ "$#" = "0" ]; then
        pushd ${HOME} >/dev/null
    elif [ -f "${1}" ]; then
        ${EDITOR:-nano} ${1}
    else
        pushd "$1" >/dev/null
    fi
}

function bd() {
    if [ "$#" = "0" ]; then
        popd >/dev/null
    else
        for i in $(seq ${1}); do
            popd >/dev/null
        done
    fi
}

#
# Checksums
#
# https://checksum.sh
function checksum() {
    local s
    s=$(curl -fsSL "$1")

    local h
    if command -v shasum >/dev/null ; then
        h=shasum
    else
        h=sha1sum
    fi

    if [ ! "$2" ] ; then
        printf %s\\n "$s" | "$h" | awk '{print $1}'
        return 1;
    fi

    printf %s\\n "$s" | "$h" --check --status <(printf '%s  -\n' "$2") || {
        echo "checksum failed" >&2;
        return 1;
    }

    printf %s\\n "$s"
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

#
# SSH-Agent Management
#
killall-ssh-agents() {
    ps aux | grep "$USER.*ssh-agent" | awk '{ print $2 }' | xargs kill
}
