let
  pkgs = import <nixpkgs> { };

  haskellPackages = hs: with hs; [ hoogle hlint stylish-haskell hspec ];

  pre-commit = pkgs.writeScriptBin "pre-commit" ''
    #!${pkgs.stdenv.shell}
    # hs-lint
    echo "hs files..."
    ${pkgs.stylish-haskell}/bin/stylish-haskell \
      -i $(${pkgs.fd}/bin/fd hs --type file | xargs) || exit 1
    ${pkgs.hlint}/bin/hlint . || exit 1
    echo "...OK"

    echo "nix files..."
    ${pkgs.nixfmt}/bin/nixfmt --width=80 $(fd '.nix' --type file | xargs) || exit 1
    echo "...OK"
  '';

in pkgs.mkShell {

  buildInputs =
    [ pkgs.nixfmt pkgs.fd pre-commit (pkgs.ghc.withPackages haskellPackages) ];

  shellHook = ''
    ln -fs ${pre-commit}/bin/pre-commit ./.git/hooks/pre-commit
  '';

}
