# Latest version
# import (builtins.fetchTarball "https://github.com/timbertson/opam2nix/archive/v1.tar.gz") {}

# For NixOS 21.11
import (
  with import <nixpkgs> {};
  fetchFromGitHub {
    owner = "timbertson";
    repo = "opam2nix";
    rev = "2184246b338b98abd301e4aeca6e8a9d5f5ce80c";
    sha256 = "1l9bf9svpbgnfyfcxrrpajgilvlsdvfmyayij59fx0rc9p8bhhz5";
  }
) {}

# For NixOS 21.05
# import (
#   with import <nixpkgs> {};
#   fetchFromGitHub {
#     owner = "timbertson";
#     repo = "opam2nix";
#     rev = "48e98dce385cee4a615445249d4c0aaa58596872";
#     sha256 = "1drw254961xqg2gz7pbkgpc81gxwhq11bzdp83kzc0sdj5hixsxd";
#   }
# ) {}

# For NixOS 20.09:
# import (
#   with import <nixpkgs> {};
#   fetchFromGitHub {
#     owner = "timbertson";
#     repo = "opam2nix";
#     rev = "90f8d627501f39e16e20c22161ec7d0aec10af32";
#     sha256 = "1d60adr1z26mdn4hp146263f5fhb66yhwdfsf9m9lgpybbra7gkh";
#   }
# ) {}
