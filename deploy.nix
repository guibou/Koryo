with import <nixpkgs> {};
let
  reflexProject = import ./.;
in
rec {
  rp = reflexProject;
  runKoryo = writeScript "run-koryo" ''
     ${python3}/bin/python3 -m http.server --directory ${client} 3003&
     ${python3}/bin/python3 -m http.server  --directory ${client} 3004&
  '';

  server = reflexProject.ghc.Koryo;

  client = runCommand "koryo-client" {} ''
    mkdir -p $out

    cp ${reflexProject.ghcjs.Koryo}/bin/ui.jsexe/* $out
    cp ${./.}/images/*.png $out
'';
}
