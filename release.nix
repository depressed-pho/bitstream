{ compiler ? (pkgs: pkgs.haskellPackages) }:
let
  pkgs = import ./pkgs.nix { inherit config; };
  config = {
    packageOverrides = pkgs: rec {
      bitstream = pkgs.callPackage ./derivations/bitstream.nix {};
      haskellPackages = (compiler pkgs).override { overrides = haskOverrides; };
    };
  };
  gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner  = "siers";
    repo   = "nix-gitignore";
    rev    = "ce0778ddd8b1f5f92d26480c21706b51b1af9166";
    sha256 = "1d7ab78i2k13lffskb23x8b5h24x7wkdmpvmria1v3wb9pcpkg2w";
  }) {};
  ignore = gitignore.gitignoreSourceAux ''
    .stack-work
    dist
    dist-newstyle
    .ghc.environment*
    '';
  haskOverrides = new: old: rec {
    bitstream = new.callCabal2nix "bitstream" (ignore ./.) {};
  };
in {
  inherit pkgs;
  packages = { inherit (pkgs.haskellPackages) bitstream; };
}
