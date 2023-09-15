{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    treefmt-nix,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      perSystem = {
        self',
        pkgs,
        config,
        ...
      }: {
        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://zero-to-flakes.com/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://zero-to-flakes.com/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # packages = {
          #   aeson.source = "1.5.0.0"; # Hackage version override
          #   shower.source = inputs.shower;
          # };
          # settings = {
          #   aeson = {
          #     check = false;
          #   };
          #   relude = {
          #     haddock = false;
          #     broken = false;
          #   };
          # };

          devShell = {
            # Enabled by default
            enable = true;

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            tools = hp: {ghcid = null;};

            hlsCheck.enable = false;
          };

          autoWire = ["packages" "apps" "checks"]; # Wire all but the devShell
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs =
            [
              pkgs.lua5_4
              config.treefmt.build.wrapper
            ]
            ++ builtins.attrValues config.treefmt.build.programs;
        };

        treefmt.config = {
          projectRootFile = "flake.nix";
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.alejandra.enable = true;
          programs.cabal-fmt.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.gevrek;
      };
    };
}
