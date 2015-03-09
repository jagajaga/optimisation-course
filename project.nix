{ mkDerivation, base, mtl, stdenv }:
mkDerivation {
  pname = "optimisation-course";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ base mtl ];
  license = stdenv.lib.licenses.unfree;
}
