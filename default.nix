{
  reflex-platform ? import ./dep/reflex-platform { system = builtins.currentSystem; }
}: reflex-platform.project ({ pkgs, thunkSource, ... }: {
  name = "reflex-react";
  src = ./.;
  ghcjs-compiler-nix-name = "ghcjs8107JSString"; #TODO: This must be default
  compiler-nix-name = "ghc8107Splices"; #TODO: This must be default
  shells = ps: with ps; [ reflex-react ];
  inputThunks = [
    ./dep/react-haskell
    {
      thunk = ./dep/jsaddle;
      subdirs = [
        "jsaddle"
        "jsaddle-warp"
      ];
    }
    ./dep/jsaddle-dom
  ];
  overrides = [
    ({ config, pkgs, lib, ... }: {

    })
  ];
})
