let
  pkgs = import <nixpkgs> {};
  callPackage = pkgs.haskellPackages.callPackage;
  pkg = callPackage ./. {
  };
  inherit (pkgs.haskell.lib) addBuildTools;
in
  (addBuildTools pkg (with pkgs.haskellPackages; [ cabal-install ghc-core ])).env
