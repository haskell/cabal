# cabal command line completion
# Copyright 2007-2008 "Lennart Kolmodin" <kolmodin@gentoo.org>
#                     "Duncan Coutts"     <dcoutts@gentoo.org>
#

# returns packages from cabal list
# usage: _cabal_packages [packages] [versions] [installed]
_cabal_packages()
{
    local packages=no # print package names with versions? ie. foo
    local versions=no # print packages and versions? ie. foo-0.42
    local installed=no # only print installed packages?

    while [[ -n "$1" ]]; do
        case "$1" in
            packages)
                packages=yes ;;
            versions)
                versions=yes ;;
            installed)
                installed=yes ;;
        esac
        shift
    done

    if [[ "$packages" == "no" && "$versions" == "no" ]]; then
        # nothing to print
        # set sensible default, print only packages
        packages=yes
    fi

    local cmd="cabal list --simple-output"
    if [[ "$installed" == "yes" ]]; then
        cmd="$cmd --installed"
    fi

    # save 'cabal list' output to a temporary file
    # putting it in a variable would mess up the lines
    local tmp=$( mktemp /tmp/cabal_completion.XXXXXX )
    $cmd > $tmp

    if [[ "$packages" == "yes" ]]; then
        # print only the names
        cat "$tmp" | cut -d' ' -f1 | uniq
    fi

    if [[ "$versions" == "yes" ]]; then
        # join the name and the version with a dash
        cat "$tmp" | sed -e "s: :-:"
    fi

    rm -f "$tmp"
}

_cabal_commands()
{
    # this is already quite fast, and caching does not give a speedup
    # 3-4ms
    for word in $( cabal --list-options ); do
        case $word in
            -*)
                # ignore flags
                continue;;
            *)
                echo $word ;;
        esac
    done
}

_cabal()
{
    # get the word currently being completed
    local cur
    cur=${COMP_WORDS[$COMP_CWORD]}

    # create a command line to run
    local cmd
    # copy all words the user has entered
    cmd=( ${COMP_WORDS[@]} )

    # the word currently beeing completed
    local ccword
    ccword=cmd[${COMP_CWORD}]

    # replace the current word with --list-options
    cmd[${COMP_CWORD}]="--list-options"

    # find the action being completed
    local action="unknown"
    for cword in ${COMP_WORDS[*]}; do
        for act in $( _cabal_commands ); do
            if [[ "$cword" == "$act" ]]; then
                action=$act
            fi
        done
    done

    # if non empty, we will pass this to _cabal_packages and add the result
    # to the completing words
    local complete_packages
    for cword in ${COMP_WORDS[*]}; do
        case $cword in
            --installed)
                # the user is interested only in installed packages
                complete_packages="$complete_packages installed"
        esac
    done

    case $action in
        install|list|upgrade|fetch)
            if [[ "$cword" != -* ]]; then
                # don't complete with packages if the user is trying to
                # complete a flag
                complete_packages="$complete_packages packages"
                if [[ "$cword" == *- ]]; then
                    # if the user tries to complete with a version, help by
                    # completing them too
                    complete_packages="$complete_packages versions"
                fi
            fi ;;
    esac

    # the resulting completions should be put into this array
    COMPREPLY=( $( compgen -W "$( ${cmd[@]} )" -- $cur ) )

    if [[ -n "$complete_packages" ]]; then
        COMPREPLY+=( $( compgen -W "$( _cabal_packages $complete_packages )" -- $cur ) )
    fi
}

complete -F _cabal -o default cabal
