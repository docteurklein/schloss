{ mkDerivation, async, base, bytestring, fetchgit, hasql, hspec
, mtl, postgres-options, postgresql-libpq, stdenv, text
, tmp-postgres
}:
mkDerivation {
  pname = "hasql-notifications";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/jfischoff/hasql-notifications";
    sha256 = "07scbcbr0nhvnn044m1v8ymrb367bdkibp827c7fn5jzabldijg1";
    rev = "ca0eeb8851a3721eaecc9b683e522726a0b86ac6";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring hasql mtl postgresql-libpq
  ];
  testHaskellDepends = [
    async base bytestring hasql hspec postgres-options postgresql-libpq
    text tmp-postgres
  ];
  homepage = "https://github.com/cocreature/hasql-notifications#readme";
  description = "Support for PostgreSQL notifications in hasql";
  license = stdenv.lib.licenses.bsd3;
}
