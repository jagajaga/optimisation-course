{ mkDerivation, base, Chart-cairo, mtl, stdenv, Chart }:
mkDerivation {
  pname = "optimisation-course";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ base Chart-cairo mtl Chart ];
  license = stdenv.lib.licenses.unfree;
}
