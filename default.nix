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
      }
  )];
}).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    Koryo = ./.;
  };

  shells = {
    ghc = ["Koryo"];
    ghcjs = ["Koryo"];
  };
})
