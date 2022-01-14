# To upgrade nix, run
# nix-shell -p niv --run "niv upgrade"
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.awscli
    pkgs.bash
    pkgs.goaccess
    pkgs.imagemagick
    pkgs.jdk
    pkgs.jq
    pkgs.terraform_0_14
  ];
}
