{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let haskellPackages' = haskellngPackages.override {
      overrides = self: super: {
        Chart = super.Chart.override {
          mkDerivation = (attrs: self.mkDerivation (attrs // { 
            src = ./chart/.;
          }));
        };
      };
    };
in
haskellPackages'.callPackage ./project.nix { }
