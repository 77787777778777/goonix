{
  description = "goonix: a fork of GNU Guix, built and pinned with Nix flakes";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  # goonix: the daemon is also a haskell-flake project.
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.haskell-flake.url = "github:srid/haskell-flake";

  outputs =
    inputs@{ self, nixpkgs, ... }:
    let
      lib = nixpkgs.lib;
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      rev = self.shortRev or self.dirtyShortRev or "unknown";
      baseVersion = "1.5.0";

      # The commit is recorded because a flake source has no .git directory.
      # The recipe retains the upstream daemon by default until its own switch.
      mkGoonix = pkgs:
        pkgs.guix.overrideAttrs (old: {
          pname = "goonix";
          version = "${baseVersion}-goonix.${rev}";
          src = self;
          # goonix: make the compiler available but leave the daemon choice alone.
          nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.haskell.packages.ghc910.ghc ];
          meta = old.meta // {
            description = "Fork of GNU Guix, a functional package manager with a Scheme interface";
            homepage = "https://github.com/hurdkit/goonix";
            changelog = "https://github.com/hurdkit/goonix/blob/goonix/NEWS";
            maintainers = [ ];
          };
        });

      # goonix: compose the ordinary package with the standalone daemon project.
      composed = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
        systems = systems;
        imports = [ inputs.haskell-flake.flakeModule ];
        perSystem = { system, config, ... }: {
          config = {
          haskellProjects.default = {
            basePackages = nixpkgs.legacyPackages.${system}.haskell.packages.ghc910;
            projectRoot = ./daemon;
            packages.guix-daemon-hs.source = ./daemon;
            autoWire = [ ];
            devShell = {
              hoogle = false;
              tools = hp: {
                haskell-language-server = null;
                hlint = null;
                ormolu = null;
                ghcid = null;
              };
              mkShellArgs.nativeBuildInputs = [
                nixpkgs.legacyPackages.${system}.cabal-install
              ];
            };
          };
            packages.guix-daemon-hs = config.haskellProjects.default.outputs.packages.guix-daemon-hs.package;
            devShells.daemon = config.haskellProjects.default.outputs.devShell;
          };
        };
      };
    in
    lib.recursiveUpdate composed {
      packages = forAllSystems (pkgs: rec {
        goonix = mkGoonix pkgs;
        default = goonix;
        goonix-cxx-daemon = goonix.overrideAttrs (old: {
          configureFlags = old.configureFlags ++ [ "--disable-haskell-daemon" ];
        });
      });
      overlays.default = final: prev: { goonix = mkGoonix prev; };
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell { inputsFrom = [ (mkGoonix pkgs) ]; };
      });
    };
}
