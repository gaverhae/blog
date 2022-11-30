# To upgrade nix, run
# nix-shell -p niv --run "niv upgrade"
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.bash
    (pkgs.leiningen.override { jdk = pkgs.openjdk11_headless; })
  ];
}
