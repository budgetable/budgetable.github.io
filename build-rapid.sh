#!/bin/bash

nix-shell --command 'cabal build --ghcjs' && \
    cp dist-newstyle/build/*/*/*/x/*/build/*/budgetable-app.jsexe/all.js ./ && \
    sed --in-place -e 's/all.min.js/all.js/' index.html
