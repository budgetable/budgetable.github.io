#! /bin/bash

nix-shell -p stylish-haskell --command 'find app/ src/ -type f -name "*.hs" | xargs stylish-haskell -i'
nix-shell -p hlint --command 'hlint src/; hlint app/'
nix-shell -p silver-searcher --command 'ag "FIXME|TODO|NOTE|Debug|trace|putStrLn|print |log |warn |error " ./src/ ./app/ ./index.html'

