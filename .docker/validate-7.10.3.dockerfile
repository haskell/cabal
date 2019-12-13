FROM    phadej/ghc:7.10.3-bionic

# We need newer compiler, to install cabal-plan
RUN     apt-get update
RUN     apt-get install -y ghc-8.6.5

# Install cabal-plan
RUN     cabal v2-update
RUN     cabal v2-install -w /opt/ghc/8.6.5/bin/ghc-8.6.5 cabal-plan --constraint 'cabal-plan ^>=0.6'

# Remove ghc-8.6.5, so it doesn't interfere
RUN     apt-get remove -y ghc-8.6.5

# We install happy, so it's in the store; we (hopefully) don't use it directly.
RUN     cabal v2-install happy --constraint 'happy ^>=1.19.12'

# Install some other dependencies
# Remove $HOME/.ghc so there aren't any environments
RUN     cabal v2-install -w ghc-7.10.3 --lib \
          aeson \
          async \
          base-compat \
          base16-bytestring \
          base64-bytestring \
          cryptohash-sha256 \
          Diff \
          echo \
          ed25519 \
          edit-distance \
          haskell-lexer \
          HTTP \
          network \
          optparse-applicative \
          pretty-show \
          regex-compat-tdfa \
          regex-tdfa \
          resolv \
          statistics \
          tar \
          tasty \
          tasty-golden \
          tasty-hunit \
          tasty-quickcheck \
          tree-diff \
          zlib \
        && rm -rf $HOME/.ghc

# Validate
WORKDIR /build
COPY    . /build
RUN     sh ./validate.sh -w ghc-7.10.3 -v
