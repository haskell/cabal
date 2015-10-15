. ../common.sh

# Create the sandbox
cabal sandbox init

# Add the sources
cabal sandbox add-source p
cabal sandbox add-source q

# Install the second package
cabal install q
