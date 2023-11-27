{
  description = ''
    My solutions to the 2019 Advent of Code.
  '';
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.2305.tar.gz";
  };
  outputs = { self, ... }@inputs: {
    devShells.aarch64-darwin.default = let
      pkgs = inputs.nixpkgs.legacyPackages.aarch64-darwin;
    in pkgs.mkShell {
      packages = with pkgs; [ janet jpm ];
    };
  };
} 
