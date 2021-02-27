self: super: rec {
  all-cabal-hashes = super.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/354e62f.tar.gz";
    sha256 = "0asifk8jysdfwn4zlbj2ny05jwz5hhs6abgjm7sv8pc3w0zlkmi9";
  };

  haskellPackages = super.haskellPackages.override {
    overrides = selfh: superh: with super.haskell.lib; rec {
      ## haskell-language-server
      hls-src = super.fetchzip {
        url = "https://github.com/haskell/haskell-language-server/archive/1.0.0.tar.gz";
        sha256 = "0p0rhhc6pldzan85qp3nhc54zwabah8r3dvxdzw49i32dvy4xxgs";
      };

      # Shared executable is needed for template haskell (SW-3716).
      # https://github.com/haskell/haskell-language-server/issues/1160#issuecomment-756566273
      haskell-language-server = enableSharedExecutables (dontCheck (superh.callCabal2nixWithOptions "haskell-language-server"
        hls-src # fourmolo and brittany are disabled because not used in the codebase.
        "-f -fourmolu -f -brittany"
        { }));

      # Disable tests because of broken tasty discover
      with-utf8 = dontCheck superh.with-utf8;
      heapsize = superh.callHackage "heapsize" "0.3.0.1" { };

      hlint = enableCabalFlag superh.hlint "ghc-lib";

      # Common
      hiedb = dontCheck (superh.callHackage "hiedb" "0.3.0.1" { });

      lsp = superh.callHackage "lsp" "1.1.1.0" { };
      lsp-types = superh.callHackage "lsp-types" "1.1.0.0" { };
      haskell-lsp = superh.callHackage "haskell-lsp" "0.23.0.0" { };
      haskell-lsp-types = superh.callHackage "haskell-lsp-types" "0.23.0.0" { };
      lsp-test = dontCheck (superh.callHackage "lsp-test" "0.13.0.0" { });
      implicit-hie-cradle = superh.callHackage "implicit-hie-cradle" "0.3.0.2" { };
      implicit-hie = superh.callHackage "implicit-hie" "0.1.2.5" { };
      ghc-exactprint = superh.callHackage "ghc-exactprint" "0.6.3.4" { };
      ghc-lib = superh.callHackage "ghc-lib" "8.10.4.20210206" { };
      ghc-lib-parser = superh.callHackage "ghc-lib-parser" "8.10.4.20210206" { };
      ghc-lib-parser-ex = disableCabalFlag (addBuildDepends superh.ghc-lib-parser-ex [ selfh.ghc-lib-parser ]) "auto";

      ghcide = dontCheck (superh.callCabal2nix "ghcide" "${hls-src}/ghcide" { });
      shake-bench = superh.callCabal2nix "shake-bench" "${hls-src}/shake-bench" { };
      hie-compat = superh.callCabal2nix "hie-compat" "${hls-src}/hie-compat" { };

      # Plugins
      apply-refact = superh.callHackage "apply-refact" "0.9.0.0" { };

      hls-plugin-api = superh.callCabal2nix "hls-plugin-api" "${hls-src}/hls-plugin-api" { };
      hls-hlint-plugin = dontCheck (superh.callCabal2nixWithOptions "hls-hlint-plugin" "${hls-src}/plugins/hls-hlint-plugin" "-f ghc-lib" { });
      hls-class-plugin = superh.callCabal2nix "hls-class-plugin" "${hls-src}/plugins/hls-class-plugin" { };
      hls-tactics-plugin = dontCheck (superh.callCabal2nix "hls-tactics-plugin" "${hls-src}/plugins/hls-tactics-plugin" { });
      hls-splice-plugin = superh.callCabal2nix "hls-splice-plugin" "${hls-src}/plugins/hls-splice-plugin" { };
      hls-eval-plugin = superh.callCabal2nix "hls-eval-plugin" "${hls-src}/plugins/hls-eval-plugin" { };
      hls-explicit-imports-plugin = superh.callCabal2nix "hls-explicit-imports-plugin" "${hls-src}/plugins/hls-explicit-imports-plugin" { };
      hls-haddock-comments-plugin = superh.callCabal2nix "hls-haddock-comments-plugin" "${hls-src}/plugins/hls-haddock-comments-plugin" { };
      hls-retrie-plugin = superh.callCabal2nix "hls-retrie-plugin" "${hls-src}/plugins/hls-retrie-plugin" { };
    };
  };
}
