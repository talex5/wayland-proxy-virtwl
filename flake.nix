{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) ocamlPackages;
      in
      {
        packages = {
          proxy =
            # Based on build rule in nixpkgs, by qyliss and sternenseemann
            ocamlPackages.buildDunePackage rec {
              pname = "wayland-proxy-virtwl";
              version = "dev";

              src = ./.;

              nativeBuildInputs = [
                pkgs.pkg-config
              ];

              buildInputs = [ pkgs.libdrm ] ++ (with ocamlPackages; [
                dune-configurator
                eio_main
                ppx_cstruct
                xmlm  #wayland    (vendored)
                cmdliner
                logs
                ppx_cstruct
              ]);
            };

          default = self.packages.${system}.proxy;
        };
      }
    );
}
