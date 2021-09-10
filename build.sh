#!/bin/bash

nix-build && \
    cp -f result/bin/budgetable-app.jsexe/all.min.js ./ &&
    sed --in-place -e 's/all.js/all.min.js/' index.html
