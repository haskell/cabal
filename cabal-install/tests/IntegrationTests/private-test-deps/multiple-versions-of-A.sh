. ./common.sh

# Create the sandbox
cabal sandbox init >/dev/null

# Add additional sources
cabal sandbox add-source deps/A-1 >/dev/null
cabal sandbox add-source deps/T   >/dev/null

# Install 
cabal install --enable-tests >/dev/null

# Run the test
# We don't know the name of the sandbox dir,
# but there will only be one so we can use '*'
dist/*/build/T-test/T-test
