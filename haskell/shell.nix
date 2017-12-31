let
  compiler = "ghc822";
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compiler}" = super.haskell.packages.${compiler}.override {
            overrides = self: super : {
              feed = self.callPackage ../overrides/feed-1.0.0.0.nix {};
              # use slightly newer pkg that fixes tests for ghc822
              reroute = self.callPackage ../overrides/reroute-0.4.1.0.nix {};
            };
          };
        };
      };
    };
  };
  pkgs = import ../nixpkgs.nix { inherit config; };
in
  (pkgs.haskell.packages.${compiler}.callPackage ./. {}).env
