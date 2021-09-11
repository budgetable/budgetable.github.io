#! /bin/bash

nix-shell -p stylish-haskell --command 'stylish-haskell -i src/**/*.hs && stylish-haskell -i src/*.hs && stylish-haskell -i app/*.hs'
nix-shell -p hlint --command 'hlint src/; hlint app/'
nix-shell -p silver-searcher --command 'ag "FIXME|TODO|NOTE|Debug|trace" ./src/ ./app/'

