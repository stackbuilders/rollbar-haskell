{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-22.11-small";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "rollbar-haskell";
      shell = ./shell.nix;
    };
}
