{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation rec {
  name = "problema-de-transpose";

  src = ./.;

  buildInputs = [ cmake boost ];

  installPhase = ''
    mkdir -p $out/bin
    cp transportation  $out/bin
  '';
}
