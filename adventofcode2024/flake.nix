{
  description = ''
    My solutions to the 2023 Advent of Code.
  '';
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.2405.tar.gz";
    flake-utils.url = "github:numtide/flake-utils";
    janet2nix = {
      url = "git+ssh://git@github.com/alan-strohm/janet2nix";
      #url = "git+file:///home/astrohm/src/github.com/alan-strohm/janet2nix";
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
    heap = pkgs.mkJanetPackage {
      name = "heap";
      url = "https://github.com/ianthehenry/heap.git";
      rev = "ff5e69ded337c0ecd0f16492ec987dc8e120d295";
      withJanetPackages = with pkgs.janetPackages; [ judge ];
    };
    myEnv = pkgs.mkJanetTree {
      name = "adventofcode2024";
      withJanetPackages = with pkgs.janetPackages; [ heap judge spork jaylib ];
    };
  in rec {
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [ myEnv entr ];
      LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath [ libGL ];
    };
  });
} 
