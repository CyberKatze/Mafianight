{
  description = "A Nix-flake-based Haskell development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    playwright.url = "github:pietdevries94/playwright-web-flake"; # To set a custom version: "github:pietdevries94/playwright-web-flake/1.37.1"

  };


  outputs = { self, nixpkgs, flake-utils, playwright }:
    # eachDefaultSystem argument is lambda fucntion (system: ...) that will be called with different systems  
    # nix flake show -> for more info
    # nix repl  -> :lf flake.nix -> for debugging
    flake-utils.lib.eachDefaultSystem (system: 
    let
      overlay = final: prev: {
        inherit (playwright.packages.${system}) playwright-test playwright-driver;
      };
      pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      commonPackages = pkgs : with pkgs; [
              haskell.compiler.ghc92
              haskellPackages.yesod-bin
              zlib
              nodejs_22
              nodejs_22.pkgs.pnpm
              postgresql_15
              pgcli
              playwright-test
              gnumake

      ];

      stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

    in
      {
        # dev environment that direnv use = `nix develop`
        devShells ={ 
          default = pkgs.mkShell {
            buildInputs = [stack-wrapped] ++ (commonPackages pkgs);
            NIX_PATH = "nixpkgs=" + pkgs.path;
            PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright-driver.browsers;
            PLAYWRIGHT_BROWSERS_VERSION = pkgs.playwright-driver.version;
            PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true";
          };
        };
        
        # rec make it recursive and can reuse attributes
        packages = rec {
          # pull the nix docker from dockerhub
          # flakeImage = pkgs.dockerTools.pullImage {
          #   imageName = "nixpkgs/nix-flakes";
          #   imageDigest = "sha256:653ac11d23bbe5b9693f156cafeb97c7e8000b187d1bafec3798f6c92238fde2";
          #   sha256 = "15543hvgw2g8aadkx335pprrxq3ldcv93a9qq9c4am0rbkw8prrw";
          #   finalImageName = "nixpkgs/nix-flakes";
          #   finalImageTag = "nixos-21.11";
          # };

          backend = pkgs.haskell.lib.buildStackProject {
                  name = "backend";
                  src = ./server;
                  ghc = pkgs.haskell.compiler.ghc92;
                  buildInputs = [
                  stack-wrapped
                  pkgs.postgresql_15
                  pkgs.zlib
                  ];
                };
          # use the nixFromDockerHub and add commonPackages to it
          backendImage = pkgs.dockerTools.buildImageWithNixDb {
            name = "backend";
            tag = "0.1.0";
            runAsRoot = ''
              mkdir /app
              cp -r ${./.} /app
              '';
            copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [
             backend
             pkgs.bashInteractive
            ] ; 
            pathsToLink = [ "/bin" ];
            };
            config = { 
              CMD = [ "/bin/bash" ];
            };

          };
        };
        });

}

