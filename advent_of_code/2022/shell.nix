# To upgrade nix, run
# nix-shell -p niv --run "niv upgrade"
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  jdk = pkgs.jdk;
in
pkgs.mkShell {
  buildInputs = [
    pkgs.bash
    (pkgs.leiningen.override { jdk = jdk; })
    jdk
  ];
}
