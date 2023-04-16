{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    opam2nix = {
      url = "github:talex5/opam2nix/flakes";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, opam2nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        selection =
          opam2nix.packages.${system}.opam2nix.build {
            ocaml = pkgs.ocaml-ng.ocamlPackages_5_0.ocaml;
            selection = ./nix/opam-selection.nix;
            src = ./.;
          };
      in
      {
        packages.default = selection.wayland-proxy-virtwl;
      }
    );
}
