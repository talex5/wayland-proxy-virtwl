with import <nixpkgs> {};
let
  ocaml = ocaml-ng.ocamlPackages_4_12.ocaml;
  opam2nix = import ./nix/opam2nix.nix;
  selection = opam2nix.build {
    inherit ocaml;
    selection = ./nix/opam-selection.nix;
    src = ./.;
  };
in
selection.wayland-proxy-virtwl
