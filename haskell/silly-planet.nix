{ compiler }:

let
  config = {
    packageOverrides = pkgs: rec {

      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          feed = self.callPackage ../overrides/feed-1.0.0.0.nix {};
          # use slightly newer pkg that fixes tests for ghc822
          reroute = self.callPackage ../overrides/reroute-0.4.1.0.nix {};

          silly-planet = pkgs.haskell.lib.overrideCabal
            (self.callPackage ./default.nix {})
            (oldDerivation: {
              enableSharedExecutables = false;
              enableSharedLibraries = false;
            });
        };
      };
    };
  };
  pkgs = import ../nixpkgs.nix { inherit config; };
in {
  silly-planet = pkgs.haskellPackages.silly-planet;
}
