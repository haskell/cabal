. ../common.sh

# Create the sandbox
cabal sandbox init > /dev/null

# Add one source
cabal sandbox add-source p > /dev/null

# Remove a source that exists on disk, but is not registered
cabal sandbox delete-source q
