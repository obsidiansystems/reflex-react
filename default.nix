{
  reflex-platform ? import ./dep/reflex-platform { system = builtins.currentSystem; }
}: (reflex-platform.project ({ pkgs, thunkSource, ... }: {
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
    {
      thunk = ./dep/reflex-dom;
      subdirs = [
        "reflex-dom-core"
        "reflex-dom"
      ];
    }
    ./dep/jsaddle-dom
  ];
  overrides = [
    ({ config, pkgs, lib, ... }: {

    })
  ];
})).extend (self: super: {
  shells = super.shells // {
     ghc = self.shell-driver {
       crossBuilds = [ ];
       buildInputs = with self.pkgs; [ nodejs ];
     };
  };
})
