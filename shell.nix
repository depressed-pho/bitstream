{ compiler ? (pkgs: pkgs.haskellPackages) }:
let
  release = import ./release.nix { inherit compiler; };
  pkgs = release.pkgs;
in pkgs.haskellPackages.shellFor {
  nativeBuildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghcid
  ];
  packages = _: pkgs.lib.attrValues release.packages;
}
