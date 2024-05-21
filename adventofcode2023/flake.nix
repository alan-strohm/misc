{
  description = ''
    My solutions to the 2023 Advent of Code.
  '';
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.0.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
    janet2nix = {
      url = "git+ssh://git@github.com/alan-strohm/janet2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, flake-utils, ... }@inputs:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = inputs.janet2nix.overlays.${system};
    };
    myEnv = pkgs.mkJanetTree {
      name = "adventofcode2023";
      withJanetPackages = with pkgs.janetPackages; [ judge ];
    };
  in rec {
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [ myEnv ];
    };
  });
} 
