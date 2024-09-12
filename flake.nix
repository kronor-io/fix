{
  description = "fix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    quickfix.url = "github:quickfix/quickfix";
    quickfix.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , quickfix
    , validity
    , safe-coloured-text
    , autodocodec
    , fast-myers-diff
    , sydtest
    , opt-env-conf
    , dekking
    , weeder-nix
    }:
    let
      system = "x86_64-linux";
      nixpkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
      pkgs = nixpkgsFor nixpkgs;
      allOverrides = pkgs.lib.composeManyExtensions [
        (pkgs.callPackage (validity + "/nix/overrides.nix") { })
        (pkgs.callPackage (autodocodec + "/nix/overrides.nix") { })
        (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
        (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
        (pkgs.callPackage (sydtest + "/nix/overrides.nix") { })
        (pkgs.callPackage (opt-env-conf + "/nix/overrides.nix") { })
        (pkgs.callPackage (dekking + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      haskellPackagesFor = nixpkgs: (nixpkgsFor nixpkgs).haskellPackages.extend allOverrides;
      haskellPackages = haskellPackagesFor nixpkgs;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = haskellPackages.fixRelease;
      checks.${system} = {
        coverage-report = haskellPackages.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "fix-core"
            "fix-spec"
          ];
          coverage = [
            "fix-core-gen"
            "fix-spec-gen"
          ];
        };
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = builtins.attrNames haskellPackages.fixPackages;
          inherit haskellPackages;
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = haskellPackages.shellFor {
        name = "fix-shell";
        packages = p: builtins.attrValues p.fixPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          cabal-install
          zlib
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        # FIX_SPEC_DIR = "${quickfix}/spec";
        FIX_SPEC_FILE = "./360t.xml";
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
