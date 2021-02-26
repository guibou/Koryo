let
  rev = "f019863";
  sha256 = "146xfjqdwd55s9jg1ggi6akcxxxd5c0pvc4bpjx3whwiikpcv8y4";
in
import (builtins.fetchTarball {
  url = "https://github.com/reflex-frp/reflex-platform/archive/${rev}.tar.gz";
  inherit sha256;
})
