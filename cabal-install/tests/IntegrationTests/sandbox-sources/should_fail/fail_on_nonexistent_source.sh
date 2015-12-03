. ../common.sh

# Create the sandbox
cabal sandbox init > /dev/null

# Add the sources
cabal sandbox add-source p > /dev/null
cabal sandbox add-source q > /dev/null

# delete the directory on disk
rm -R p

# Remove the registered source which is no longer on disk
cabal sandbox delete-source p
