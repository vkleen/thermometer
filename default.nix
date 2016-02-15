{ mkDerivation, base, binary-strict, bool-extras, bytestring
, either, exceptions, mtl, optparse-applicative, serialport, stdenv
, transformers, unix
}:
mkDerivation {
  pname = "thermometer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary-strict bool-extras bytestring either exceptions mtl
    optparse-applicative serialport transformers unix
  ];
  license = stdenv.lib.licenses.unfree;
}
