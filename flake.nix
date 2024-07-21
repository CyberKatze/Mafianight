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
        commonPackages = pkgs: with pkgs; [
          haskell.compiler.ghc92
          haskellPackages.yesod-bin
          zlib
          nodejs_22
          nodejs_22.pkgs.pnpm
          postgresql_15
          pgcli
          playwright-test
          gnumake
          lsof
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
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [ stack-wrapped ] ++ (commonPackages pkgs) ++ [ pkgs.presenterm ];
            NIX_PATH = "nixpkgs=" + pkgs.path;
            PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright-driver.browsers;
            PLAYWRIGHT_BROWSERS_VERSION = pkgs.playwright-driver.version;
            PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "true";
          };
        };

        # rec make it recursive and can reuse attributes
        packages = rec {

          backend = pkgs.haskell.lib.buildStackProject {
                  name = "backend";
                  src = ./server;
                  doCheck = false;
                  ghc = pkgs.haskell.compiler.ghc92;
                  buildInputs = [
                  stack-wrapped
                  pkgs.postgresql_15
                  pkgs.zlib
                  ];
                };
          # Wrap the backend binary in docker image  
          backendImage = pkgs.dockerTools.buildImage {
            name = "backend";
            tag = "latest";
            runAsRoot = ''
              mkdir -p /app/seeds
              cp  ${./server/seeds/roles.json} /app/seeds/roles.json
              '';
            copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [
             backend
            ] ; 
            pathsToLink = [ "/bin" ];
            };
            config = { 
              WorkingDir = "/app";
              CMD = [ "haskell-web" ];
            };

          };
        };
      });

}

