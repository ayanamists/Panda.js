{
  description = "Panda.js monorepo development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages."ghc982";
        stack-wrapped = pkgs.symlinkJoin {
                    name = "stack"; # will be available as the usual `stack` in terminal
                    paths = [ pkgs.stack ];
                    buildInputs = [ pkgs.makeWrapper ];
                    postBuild = ''
                      wrapProgram $out/bin/stack \
                        --add-flags "\
                          --no-nix \
                          --system-ghc \
                          --no-install-ghc \
                        "
                    '';
                  };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = (with pkgs; [
            # Node.js toolchain
            nodejs_20
            yarn

            # Build essentials
            pkg-config
            zlib
            gmp
            libffi

            # Development tools
            git
          ]) ++ (with hPkgs; [
            # Haskell toolchain
            ghc
            # cabal-install
            haskell-language-server
            stack-wrapped
          ]);

          shellHook = ''
            echo "🐼 Panda.js Development Environment"
            echo "=================================="
            echo "Node.js: $(node --version)"
            echo "Yarn: $(yarn --version)"
            echo "Stack: $(stack --version | head -1)"
            echo "GHC: $(ghc --version)"
            echo ""
            echo "Ready to develop!"
          '';

          # Use system GHC with stack
          STACK_SYSTEM_GHC = "1";
        };
      });
}
