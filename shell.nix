let
  pkgs = import <nixpkgs> { };

  haskellPackages = hs: with hs; [ hoogle hlint stylish-haskell hspec ];

  pre-commit = pkgs.writeScriptBin "pre-commit" ''
    !${pkgs.stdenv.shell}
    # hs-lint
    echo "auto-formatting all hs files"
    stylish-haskell -v -i $(fd hs --type file | xargs) && hlint . || exit 1

    echo "formatting all nix files"
    nixfmt --width=80 $(fd '.nix' --type file | xargs) || exit 1
  '';

in pkgs.mkShell {

  buildInputs =
    [ pkgs.nixfmt pkgs.fd pre-commit (pkgs.ghc.withPackages haskellPackages) ];

  shellHook = ''
    ln -fs ${pre-commit}/bin/pre-commit ./.git/hooks/pre-commit
  '';

}
