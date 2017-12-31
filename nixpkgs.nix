let
  bootstrap = import <nixpkgs> {};
  nixpkgsSnapshot = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    inherit (nixpkgsSnapshot) rev sha256;
  };
  pkgs = import src;
in
  pkgs
