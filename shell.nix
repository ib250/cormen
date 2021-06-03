
let
  pkgs = import <nixpkgs> {};

  haskellPackages = hs:
    with hs; [hoogle hlint stylish-haskell hspec];

in pkgs.mkShell {

  buildInputs = [
    pkgs.fd
    (pkgs.ghc.withPackages haskellPackages)
  ];

}