. ./common.sh

OUTPUT=`cabal outdated --new-freeze-file`

echo $OUTPUT | grep base || die "should have listed 'base' as outdated"
echo $OUTPUT | grep template-haskell \
    || die "should have listed 'base' as outdated"

cabal outdated --new-freeze-file --ignore=base | grep -v base \
    || die "shouldn't have listed 'base' as outdated"
cabal outdated --new-freeze-file --minor=base  | grep -v base \
    || die "shouldn't have listed 'base' as outdated"

cabal outdated --new-freeze-file --ignore=template-haskell \
    | grep -v template-haskell \
    || die "shouldn't have listed template-haskell as outdated"
cabal outdated --new-freeze-file --minor=template-haskell \
    | grep template-haskell \
    || die "should have listed template-haskell as outdated"
