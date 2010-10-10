# cabal command line completion
# Copyright 2007-2008 "Lennart Kolmodin" <kolmodin@gentoo.org>
#                     "Duncan Coutts"     <dcoutts@gentoo.org>
#

_cabal()
{
    # get the word currently being completed
    local cur
    cur=${COMP_WORDS[$COMP_CWORD]}

    # create a command line to run
    local cmd
    # copy all words the user has entered
    cmd=( ${COMP_WORDS[@]} )

    # replace the current word with --list-options
    cmd[${COMP_CWORD}]="--list-options"

    # the resulting completions should be put into this array
    COMPREPLY=( $( compgen -W "$( ${cmd[@]} )" -- $cur ) )
}

complete -F _cabal -o default cabal
