FROM haskell:8.6.5

# We need prof GHC for some tests
RUN     apt-get update
RUN     apt-get install ghc-8.6.5-prof

# Install cabal-plan
RUN     cabal v2-update
RUN     cabal v2-install cabal-plan --constraint 'cabal-plan ^>=0.6'

# Validate
WORKDIR /app
COPY    . /app
RUN     sh ./validate.sh -w ghc-8.6.5
