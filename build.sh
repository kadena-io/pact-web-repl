BUILDROOT="dist-newstyle"
OUTDIR=$BUILDROOT/build/x86_64-osx/ghcjs-0.2.1/pact-ghcjs-2.3.8/c/pact/build/pact/pact.jsexe
cabal --builddir=$BUILDROOT new-build pact-ghcjs
cp $OUTDIR/all.js site
