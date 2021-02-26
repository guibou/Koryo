with import <nixpkgs> {};
let
  reflexProject = import ./.;
in
rec {
  rp = reflexProject;
  ui = haskell.lib.dontHaddock reflexProject.ghcjs.Koryo;

  runKoryo = writeScript "run-koryo" ''
     ${python3}/bin/python3 -m http.server --directory ${client} 3003
  '';

  server = reflexProject.ghc.Koryo;

  client = runCommand "koryo-client" {
    buildInputs = [pkgs.closurecompiler];
  } ''
    mkdir -p $out/images/

    cp ${ui}/bin/ui.jsexe/* $out
    cp ${./images}/*.png $out/images/

    # Closure compiler pass
    cd $out
    # Advanced optimisation level does not work
    closure-compiler --js out.js > out.js.compiled
    rm out.js
    mv out.js.compiled out.js
'';
}
