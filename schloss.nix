{ mkDerivation, aeson, base, bytestring, fast-logger, generic-lens
, hasql, hasql-notifications, hasql-pool, hasql-th, http-media
, http-types, mtl, postgresql-binary, servant, servant-server
, stdenv, text, transformers, uuid, valor, wai, wai-extra, warp
}:
mkDerivation {
  pname = "schloss";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring fast-logger generic-lens hasql
    hasql-notifications hasql-pool hasql-th http-media http-types mtl
    postgresql-binary servant servant-server text transformers uuid
    valor wai wai-extra warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
