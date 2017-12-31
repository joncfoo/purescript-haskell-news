let
  compiler = "ghc822";
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compiler}" = super.haskell.packages.${compiler}.override {
            overrides = self: super : {
              feed = self.callPackage ./overrides/feed-1.0.0.0.nix {};
              # use slightly newer pkg that fixes tests for ghc822
              reroute = self.callPackage ./overrides/reroute-0.4.1.0.nix {};
            };
          };
        };
      };
    };
  };
  pkgs = import ./nixpkgs.nix { inherit config; };
  hpkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    });
  };
  hdeps = with hpkgs; [
    cabal-install
#     alex
#     c2hs
#     cpphs
#     doctest
#     happy
#     hscolour

    ghcid
    hasktags
    hlint
    hoogle
    hpack
    stylish-haskell
  ];
  deps = with pkgs; [
    binutils
    cabal2nix
    nix-prefetch-git
  ];
  silly-planet = (import ./haskell/silly-planet.nix { compiler = compiler; }).silly-planet;
in
silly-planet.env.overrideAttrs (oldEnv: {
  buildInputs = (oldEnv.buildInputs or []) ++ hdeps ++ deps;
})
