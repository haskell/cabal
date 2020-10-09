" Vim ftplugin file
" Language:	Haskell Cabal Build file
" Maintainer:	Cabal Development team <cabal-devel@haskell.org>
if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1


" dashes are ok in keywords, and occasionally a dot
setlocal iskeyword=a-z,A-Z,48-57,192-255,-,.

" comments start with --
setlocal commentstring=--\ %s
