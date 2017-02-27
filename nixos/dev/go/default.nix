let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in rec {
  goEnv = stdenv.mkDerivation rec {
    name = "go-env";
    version = "1.7";
    src = ./.;
    buildInputs = [ pkgs.go ];
  };
}
