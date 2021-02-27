let
  # nixpkgs-unstable of 2021-02-27
  rev = "3f110f988bc567af01d56838b52d567b49603b91";
  sha256 = "1cdl4g5g5x2lpw2cs4043bqzi7xdscr60mda67zdwbrbhb28k55b";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
