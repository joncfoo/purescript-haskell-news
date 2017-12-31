let
  compiler = "ghc822";
  pkgs = import ./nixpkgs.nix { };
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
  spEnv = import ./haskell/shell.nix;
in
spEnv.overrideAttrs (oldEnv: {
  buildInputs = (oldEnv.buildInputs or []) ++ hdeps ++ deps;
})
