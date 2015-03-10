{ mkDerivation, base, Chart, Chart-cairo, data-default-class, lens
, mtl, stdenv
}:
mkDerivation {
  pname = "optimisation-course";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base Chart Chart-cairo data-default-class lens mtl
  ];
  license = stdenv.lib.licenses.unfree;
}
