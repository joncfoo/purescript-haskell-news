{ mkDerivation, aeson, authenticate-oauth, base, base64-bytestring
, cmdargs, containers, feed, filepath, friendly-time, hpack
, http-client, http-client-tls, microlens-aeson, microlens-platform
, protolude, raw-strings-qq, resource-pool, simple-logger, Spock
, sqlite-simple, stdenv, text, time, wai, wai-extra, warp
}:
mkDerivation {
  pname = "silly-planet";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson authenticate-oauth base base64-bytestring cmdargs containers
    feed filepath friendly-time http-client http-client-tls
    microlens-aeson microlens-platform protolude raw-strings-qq
    resource-pool simple-logger Spock sqlite-simple text time wai
    wai-extra warp
  ];
  preConfigure = "hpack";
  description = "A simple feed aggregator";
  license = stdenv.lib.licenses.isc;
}
