{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    systems.url = "github:nix-systems/default-linux";
  };

  outputs = { self, nixpkgs, systems }:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    forEachSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) ocamlPackages;
      in
      {
        packages = {
          proxy =
            # Based on build rule in nixpkgs, by qyliss and sternenseemann
            ocamlPackages.buildDunePackage {
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
