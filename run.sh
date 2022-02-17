cabal run lang33 -- $1.33 build/tmp.hs &&
ghc build/tmp build/l33 -package mtl -o $1 &&
clear &&
./$1
