{ mkDerivation, aeson, base, binary, bytestring, http-client
, http-client-tls, http-types, mtl, stdenv, tasty, tasty-hunit
, text, text-binary, time, time-locale-compat, transformers
}:
mkDerivation {
  pname = "themoviedb";
  version = "1.1.5.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring http-client http-client-tls http-types
    mtl text text-binary time time-locale-compat transformers
  ];
  executableHaskellDepends = [
    base text time time-locale-compat transformers
  ];
  testHaskellDepends = [
    base bytestring tasty tasty-hunit text time
  ];
  homepage = "https://code.devalot.com/open/themoviedb";
  description = "Haskell API bindings for http://themoviedb.org";
  license = stdenv.lib.licenses.mit;
}
