let
  pkgs = import ./nixpkgs.nix {};

  reflex = import ./reflex.nix {
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
  };
in
rec {
  # Pure nix build of the UI.
  koryo = pkgs.haskellPackages.callCabal2nix "Koryo" ./. {};

  # Shell with cabal in order to work on the repl using WARP
  shell = koryo.env.overrideAttrs(old: {
    buildInputs = old.buildInputs ++ [pkgs.cabal-install];
  });

  # Shell with the HLS
  shell_hls = shell.overrideAttrs(old: {
    buildInputs = old.buildInputs ++ [pkgs.haskellPackages.haskell-language-server];
  });

  server = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck koryo);

  # REFLEX Plateform things
  # We only use the reflex platform for GHCJS and Android build

  # JS version using reflex plateform
  ui = reflex.project ({ pkgs, ... }: {
    useWarp = true;
    packages = {
      Koryo = pkgs.lib.sourceByRegex ./.  [
        "Koryo\.cabal$"
        "src"
        "app"
        "src/UIReflex"
        "tests"
        ".*\.hs$"
        "LICENSE"
      ];
    };

    shells = {
      ghcjs = ["Koryo"];
    };
  });

  # Contains the static images and the JS UI
  client = pkgs.runCommand "koryo-client" {
    buildInputs = [pkgs.closurecompiler];
  } ''
    mkdir -p $out/images/

    cp ${ui.ghcjs.Koryo}/bin/ui.jsexe/* $out
    cp ${./images}/*.png $out/images/

    # Closure compiler pass
    cd $out
    # Advanced optimisation level does not work
    closure-compiler --js out.js > out.js.compiled
    rm out.js
    mv out.js.compiled out.js
    '';

  # A simple wraper script which runs an http server which serve the client.
  runKoryo = pkgs.writeScript "run-koryo" ''
     ${pkgs.python3}/bin/python3 -m http.server --directory ${client} 3003
  '';
}
