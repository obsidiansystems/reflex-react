{
  project ? import ./dep/reflex-platform { system = builtins.currentSystem; }
}: project ({ pkgs, thunkSource, ... }: {
  name = "reflex-react";
  src = ./.;
  ghcjs-compiler-nix-name = "ghcjs8107JSString"; #TODO: This must be default
  compiler-nix-name = "ghc8107Splices"; #TODO: This must be default
  shells = ps: with ps; [ reflex-react ];
  inputMap = {
    "https://github.com/obsidiansystems/react-haskell.git/44647cb28ab60872dc47ef1a9d38d02a1edadd61" = thunkSource ./dep/react-haskell;
  };
  overrides = [
    ({ config, pkgs, lib, ... }: {
      packages.react-haskell = {
        flags.ghcjs = true;
        components.library = {
          preConfigure = ''
            sed -i 's/void == 0.7/void/g' "react-haskell.cabal"
            cat ./react-haskell.cabal
          '';
        };
      };
    })
  ];
})
