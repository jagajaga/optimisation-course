with import <nixpkgs> {};

with stdenv;
with pkgs;

let

  matplotlibGtk = pythonPackages.matplotlib.override {
    enableGtk2 = true;
    pygtk = pythonPackages.pyGtkGlade;
  };

in

mkDerivation rec {
    name = "Piyavskii";
    buildInputs = [ matplotlibGtk python ];
    src = ./.;
}
