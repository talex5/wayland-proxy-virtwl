To update these selections while wayland is vendored,
replace "wayland" with its dependencies in the opam file and run:

    opam2nix resolve --dest ./opam-selection.nix --ocaml-version 5.0.0 ../wayland-proxy-virtwl.opam
