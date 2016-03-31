. ../common.sh

# Create the sandbox
cabal sandbox init

# Add the sources
cabal sandbox add-source p
cabal sandbox add-source q

# delete the directory on disk
rm -R p

# Remove the registered source which is no longer on disk. cabal's handling of
# non-existent sources depends on the behavior of the directory package.
if OUTPUT=`cabal sandbox delete-source p 2>&1`; then
    # 'canonicalizePath' should always succeed with directory >= 1.2.3.0
    echo $OUTPUT | grep 'Success deleting sources: "p"' \
	|| die "Incorrect success message: $OUTPUT"
else
    echo $OUTPUT | grep 'Warning: Source directory not found for paths: "p"' \
	|| die "Incorrect failure message: $OUTPUT"
fi
