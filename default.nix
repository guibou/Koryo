(import ./reflex.nix {
  config .android_sdk.accept_license = true;
  config.allowBroken = true;

  haskellOverlays = [(
    selfPkgs: superPkgs:
    let
      pkgs = superPkgs.callPackage ({ pkgs }: pkgs) {};
    in
      {
        clay = pkgs.haskell.lib.doJailbreak superPkgs.clay;
        random-shuffle = superPkgs.callHackage "random-shuffle" "0.0.4" {};
        # websockets = superPkgs.callHackage "websockets" "0.12.5.3" {};
      }
  )];
}).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
#    common = ./common;
    frontend = ./frontend;
#    backend = ./backend;
  };

  shells = {
#    ghc = ["common" "backend" "frontend"];
#    ghcjs = ["common" "frontend"];
    ghc = ["frontend"];
    ghcjs = ["frontend"];
  };
})
