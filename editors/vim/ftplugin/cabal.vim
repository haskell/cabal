" Vim ftplugin file
" Language:	Haskell Cabal Build file
" Maintainer:	Cabal Development team <cabal-devel@haskell.org>
if exists('b:cabal_ftplugin')
	finish
endif
let b:cabal_ftplugin = 1

" dashes are ok in keywords, and also colons (for field names)
setlocal iskeyword+=-
setlocal iskeyword+=:
setlocal iskeyword+=.

" comments start with --
setlocal commentstring=--\ %s
