. ../common.sh

# Create the sandbox
cabal sandbox init > /dev/null

# Add the sources
cabal sandbox add-source p > /dev/null
cabal sandbox add-source q > /dev/null

# Remove one of the sources
cabal sandbox delete-source p q
