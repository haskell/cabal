#!/bin/sh
set -ex

setup() {
    git clone --depth=50 --branch=gh-pages \
        https://github.com/haskell/cabal-website.git ../cabal-website
    cd ../cabal-website
    openssl aes-256-cbc -K $encrypted_edaf6551664d_key \
            -iv $encrypted_edaf6551664d_iv \
            -in id_ed25519_cabal_website.aes256.enc -out id_ed25519 -d
    mv id_ed25519 ~/.ssh/id_ed25519
    mv id_ed25519_cabal_website.pub ~/.ssh/id_ed25519.pub
    chmod 400 ~/.ssh/id_ed25519
    cd -
}

deploy() {
    git config --global user.email "builds@travis-ci.org"
    git config --global user.name "Travis CI User"
    mkdir -p ../cabal-website/doc/html
    mv Cabal/dist/doc/html/Cabal ../cabal-website/doc/html/Cabal
    cd ../cabal-website
    git add .
    git commit --amend --reset-author -m "Deploy to GitHub ($(date))."
    git push --force --quiet \
               git@github.com:haskell/haskell-website.git gh-pages
}

if [ "x$TRAVIS_PULL_REQUEST" = "xfalse" -a "x$TRAVIS_BRANCH" = "xmaster" ]
then
    case "${1}" in
        "setup")
            setup
            ;;
        "deploy")
            deploy
            ;;
        *)
            echo Unknown command!
            ;;
    esac
fi
