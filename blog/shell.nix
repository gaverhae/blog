let
  pkgs = import ./nix/nixpkgs.nix;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    awscli
    bash
    curl
    goaccess
    imagemagick
    jdk
    jq
    opentofu
  ];
}
