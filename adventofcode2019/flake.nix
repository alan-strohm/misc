{
  description = ''
    My solutions to the 2019 Advent of Code.
  '';
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.2305.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, flake-utils, ... }@inputs:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = inputs.nixpkgs.legacyPackages.${system};
  in rec {
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [ janet jpm ];
    };
  });
} 
