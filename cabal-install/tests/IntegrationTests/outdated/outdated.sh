. ./common.sh

OUTPUT=`cabal outdated`

echo $OUTPUT | grep base || die "should have listed 'base' as outdated"
echo $OUTPUT | grep template-haskell \
    || die "should have listed 'template-haskell' as outdated"

OUTPUT=`cabal outdated --ignore=base`
echo $OUTPUT | grep -v base || die "shouldn't have listed 'base' as outdated"
echo $OUTPUT | grep template-haskell \
    || die "should have listed 'template-haskell' as outdated"

OUTPUT=`cabal outdated --ignore=base,template-haskell`
echo $OUTPUT | grep -v base || die "shouldn't have listed 'base' as outdated"
echo $OUTPUT | grep -v template-haskell \
    || die "shouldn't have listed 'template-haskell' as outdated"

OUTPUT=`cabal outdated --minor=base`
echo $OUTPUT | grep -v base || die "shouldn't have listed 'base' as outdated"
echo $OUTPUT | grep template-haskell \
    || die "should have listed 'template-haskell' as outdated"

OUTPUT=`cabal outdated --minor=base,template-haskell`
echo $OUTPUT | grep -v base || die "shouldn't have listed 'base' as outdated"
echo $OUTPUT | grep -v template-haskell \
    || die "shouldn't have listed 'template-haskell' as outdated"
