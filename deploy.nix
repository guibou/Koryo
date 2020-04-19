with import <nixpkgs> {};
let
  reflexProject = import ./.;
in
rec {
  runKoryo = writeScript "run-koryo" ''
     ${python3}/bin/python3 -m http.server --directory ${client} 3003&
     ${python3}/bin/python3 -m http.server  --directory ${client} 3004&
  '';

  server = reflexProject.ghc.frontend;

  client = runCommand "koryo-client" {} ''
    mkdir -p $out

    cp ${reflexProject.ghcjs.frontend}/bin/ui.jsexe/* $out
    cp ${./.}/frontend/images/*.png $out
'';
}
