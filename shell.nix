{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  buildInputs = [
    pkgs.python313
    pkgs.pandoc
    pkgs.tiddlywiki
    pkgs.python313Packages.titlecase
  ];
}
