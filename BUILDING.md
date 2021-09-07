# Building Budgetable

1. You must have [nix](https://nixos.org/download.html) installed.
2. Clone the repository (`git clone git@github.com:budgetable/budgetable.github.io.git`)
3. Build it with nix (`nix-build --arg isJS true`)
4. Copy the javascript executable (`cp result/bin/budgetable-app.jsexe/all.js ./`)
