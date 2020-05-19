with import <nixpkgs> {};
let
  reflexProject = import ./.;
in
rec {
  rp = reflexProject;
  ui = haskell.lib.dontHaddock reflexProject.ghcjs.Koryo;

  runKoryo = writeScript "run-koryo" ''
     ${python3}/bin/python3 -m http.server --directory ${client} 3003&
     ${python3}/bin/python3 -m http.server  --directory ${client} 3004&
  '';

  server = reflexProject.ghc.Koryo;

  client = runCommand "koryo-client" {
    buildInputs = [pkgs.closurecompiler];
  } ''
    mkdir -p $out

    cp ${ui}/bin/ui.jsexe/* $out
    cp ${./images}/*.png $out

    # Closure compiler pass
    cd $out
    closure-compiler --js out.js > out.js.compiled
    rm out.js
    mv out.js.compiled out.js
'';
}
