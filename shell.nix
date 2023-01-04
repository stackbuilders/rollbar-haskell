{ pkgs }:

pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghc
    pkgs.hpack
    pkgs.zlib
  ];
}
