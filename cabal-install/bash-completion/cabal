#!/bin/bash

##################################################

# cabal command line completion
#
# Copyright 2007-2008 "Lennart Kolmodin" <kolmodin@gentoo.org>
#                     "Duncan Coutts"    <dcoutts@gentoo.org>
# Copyright 2019-     "Sam Boosalis"     <samboosalis@gmail.com>
#

# Compatibility — Bash 3.
#
# OSX won't update Bash 3 (last updated circa 2009) to Bash 4,
# and we'd like this completion script to work on both Linux and Mac.
#
# For example, OSX Yosemite (released circa 2014) ships with Bash 3:
#
#  $ echo $BASH_VERSION
#  3.2
#
# While Ubuntu LTS 14.04 (a.k.a. Trusty, also released circa 2016)
# ships with the latest version, Bash 4 (updated circa 2016):
#
#  $ echo $BASH_VERSION
#  4.3
#

# Testing
#
# (1) Invoke « shellcheck »
#
#     * source: « https://github.com/koalaman/shellcheck »
#     * run:    « shellcheck ./cabal-install/bash-completion/cabal »
#
# (2) Interpret via Bash 3
#
#     * source: « https://ftp.gnu.org/gnu/bash/bash-3.2.tar.gz »
#     * run:    « bash --noprofile --norc --posix ./cabal-install/bash-completion/cabal »
#
#

##################################################
# Dependencies:

command -v cabal  >/dev/null
command -v grep   >/dev/null
command -v sed    >/dev/null

##################################################

# List project-specific (/ internal) packages:
#
#

function _cabal_list_packages ()
(
    shopt -s nullglob

    local CabalFiles
    CabalFiles=( ./*.cabal ./*/*.cabal ./*/*/*.cabal )

    for FILE in "${CabalFiles[@]}"
    do

        BASENAME=$(basename "$FILE")
        PACKAGE="${BASENAME%.cabal}"

        echo "$PACKAGE"

    done | sort | uniq
)

# NOTES
#
# [1] « "${string%suffix}" » strips « suffix » from « string »,
#     in pure Bash.
#
# [2] « done | sort | uniq » removes duplicates from the output of the for-loop.
#

##################################################

# List cabal targets by type, pass:
#
#   - ‹test-suite› for test suites
#   - ‹benchmark› for benchmarks
#   - ‹executable› for executables
#   - ‹library› for internal libraries
#   - ‹foreign-library› for foreign libraries
#   - nothing for all components.
#

function _cabal_list_targets ()
(
    shopt -s nullglob

    # ^ NOTE « _cabal_list_targets » must be a subshell to temporarily enable « nullglob ».
    #        hence, « function _ () ( ... ) » over « function _ () { ... } ».
    #        without « nullglob », if a glob-pattern fails, it becomes a literal
    #        (i.e. the string with an asterix, rather than an empty string).

    CabalComponent=${1:-library|executable|test-suite|benchmark|foreign-library}

    local CabalFiles
    CabalFiles=( ./*.cabal ./*/*.cabal ./*/*/*.cabal )

    for FILE in "${CabalFiles[@]}"
    do

        grep -E -i "^[[:space:]]*($CabalComponent)[[:space:]]" "$FILE" 2>/dev/null | sed -e "s/.* \([^ ]*\).*/\1/" | sed -e '/^$/d'

    done | sort | uniq
)

# NOTES
#
# [1] in « sed '/^$/d' »:
#
#     * « d » is the sed command to delete a line.
#     * « ^$ » is a regular expression matching only a blank line
#       (i.e. a line start followed by a line end).
#
#     dropping blank lines is necessary to ignore public « library » stanzas,
#     while still matching private « library _ » stanzas.
#
# [2]
#

#TODO# rm duplicate components and qualify with « PACKAGE: » (from basename):
#
# $ .. | sort | uniq

##################################################

# List possible targets depending on the command supplied as parameter.  The
# ideal option would be to implement this via --list-options on cabal directly.
# This is a temporary workaround.

function _cabal_targets ()

{
    local Completion

    for Completion in "$@"; do

        [ "$Completion" == new-build   ] && _cabal_list_targets              && break
        [ "$Completion" == new-repl    ] && _cabal_list_targets              && break
        [ "$Completion" == new-run     ] && _cabal_list_targets "executable" && break
        [ "$Completion" == new-test    ] && _cabal_list_targets "test-suite" && break
        [ "$Completion" == new-bench   ] && _cabal_list_targets "benchmark"  && break
        [ "$Completion" == new-haddock ] && _cabal_list_targets              && break

        [ "$Completion" == new-install ] && _cabal_list_targets "executable" && break
        # ^ Only complete for local packages (not all 1000s of remote packages).

        [ "$Completion" == build     ] && _cabal_list_targets "executable|test-suite|benchmark" && break
        [ "$Completion" == repl      ] && _cabal_list_targets "executable|test-suite|benchmark" && break
        [ "$Completion" == run       ] && _cabal_list_targets "executable"                      && break
        [ "$Completion" == test      ] && _cabal_list_targets            "test-suite"           && break
        [ "$Completion" == bench     ] && _cabal_list_targets                       "benchmark" && break

    done
}

# NOTES
#
# [1] « $@ » will be the full command-line (so far).
#
# [2]
#

##################################################

# List possible subcommands of a cabal subcommand.
#
# In example "sandbox" is a cabal subcommand that itself has subcommands. Since
# "cabal --list-options" doesn't work in such cases we have to get the list
# using other means.

function _cabal_subcommands ()

{
    local word
    for word in "$@"; do
        case "$word" in
          sandbox)
            # Get list of "cabal sandbox" subcommands from its help message.
            "$1" help sandbox |
            sed -n '1,/^Subcommands:$/d;/^Flags for sandbox:$/,$d;/^ /d;s/^\(.*\):/\1/p'
            break  # Terminate for loop.
            ;;
        esac
    done
}

##################################################

function __cabal_has_doubledash  ()

{
    local c=1
    # Ignore the last word, because it is replaced anyways.
    # This allows expansion for flags on "cabal foo --",
    # but does not try to complete after "cabal foo -- ".
    local n=$((${#COMP_WORDS[@]} - 1))
    while [ $c -lt $n ]; do
        if [ "--" = "${COMP_WORDS[c]}" ]; then
            return 0
        fi
        ((c++))
    done
    return 1
}


##################################################

function _cabal ()

{
    # no completion past cabal arguments.
    __cabal_has_doubledash && return

    # get the word currently being completed
    local CurrentWord
    CurrentWord=${COMP_WORDS[$COMP_CWORD]}

    # create a command line to run
    local CommandLine
    # copy all words the user has entered
    CommandLine=( "${COMP_WORDS[@]}" )

    # replace the current word with --list-options
    CommandLine[${COMP_CWORD}]="--list-options"

    # the resulting completions should be put into this array
    COMPREPLY=( $( compgen -W "$( eval "${CommandLine[@]}" 2>/dev/null ) $( _cabal_targets "${CommandLine[@]}" ) $( _cabal_subcommands "${COMP_WORDS[@]}" )" -- "$CurrentWord" ) )
}

# abc="a b c"
# { IFS=" " read -a ExampleArray <<< "$abc"; echo ${ExampleArray[@]}; echo ${!ExampleArray[@]}; }

##################################################

complete -F _cabal -o default cabal
