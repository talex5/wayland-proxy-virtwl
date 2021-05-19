# Latest version (for NixOS 21.05):
# import (builtins.fetchTarball "https://github.com/timbertson/opam2nix/archive/v1.tar.gz") {}

# For NixOS 20.09 (stable):
import (
  with import <nixpkgs> {};
  fetchFromGitHub {
    owner = "timbertson";
    repo = "opam2nix";
    rev = "90f8d627501f39e16e20c22161ec7d0aec10af32";
    sha256 = "1d60adr1z26mdn4hp146263f5fhb66yhwdfsf9m9lgpybbra7gkh";
  }
) {}
