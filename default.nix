with import <nixpkgs> {};

let hdeps = p: with p; [
    aeson
    authenticate-oauth
    base64-bytestring
    containers
    feed_1_0_0_0
    friendly-time
    http-client
    http-client-tls
    microlens-aeson
    microlens-platform
    microstache
    protolude
    raw-strings-qq
    time
  ];
in
stdenv.mkDerivation rec {
  name = "purescript-haskell-news";

  ghc = pkgs.haskell.packages.ghc822.ghcWithPackages hdeps;

  buildInputs = [ ghc ];

  env = buildEnv {
    name = name;
    paths = buildInputs;
  };

  shellHook = ''eval $(egrep ^export ${ghc}/bin/ghc)'';
}
