{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};
  in rec {
    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [ go gopls gotools go-tools ];
    };
    packages.default = pkgs.buildGoModule {
      name = "adventofcode2022";
      src = ./.;
      #vendorSha256 = pkgs.lib.fakeSha256;
      vendorSha256 = "sha256-lTWbCprn4B9/vFCAvEEBYOl8Dbx2AfQYm7BoDtvMmH0=";
    };
  });
}
