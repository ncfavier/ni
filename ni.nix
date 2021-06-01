{ mkDerivation, base, base64-bytestring, bytestring, connection
, containers, data-default-class, lib, transformers, unix
}:
mkDerivation {
  pname = "ni";
  version = "0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base64-bytestring bytestring connection containers
    data-default-class transformers unix
  ];
  homepage = "https://git.monade.li/ni";
  description = "A stack-based concatenative programming language";
  license = lib.licenses.isc;
}
