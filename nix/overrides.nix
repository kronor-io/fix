{ lib
, haskell
, symlinkJoin
, vcal
, callPackage
, ...
}:
with lib;
with haskell.lib;
self: super:
let
  fixPackages =
    let
      fixPkg = name:
        buildFromSdist (
          overrideCabal (self.callPackage (../${name}) { })
            (old: {
              doBenchmark = true;
              doCheck = false; # Only for coverage report
              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Werror"
                "--ghc-options=-Wno-deprecations"
              ];
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct";
            })
        );
    in
    {
      fix-core = fixPkg "fix-core";
      fix-core-gen = fixPkg "fix-core-gen";
      fix-codegen = fixPkg "fix-codegen";
      fix-spec = fixPkg "fix-spec";
    };
in
{
  inherit fixPackages;
  fixRelease = symlinkJoin {
    name = "fix-release";
    paths = attrValues self.fixPackages;
    passthru = self.fixPackages;
  };
} // fixPackages
