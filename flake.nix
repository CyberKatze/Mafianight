{
  description = "A Nix-flake-based Haskell development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            haskell.compiler.ghc92
            # (haskell-language-server.override { supportedGhcVersions = [ "92" ]; })
            haskellPackages.yesod-bin
            haskellPackages.stack
            haskellPackages.zlib
            nodejs_22
            postgresql_15
            pgcli
          ];
          NIX_PATH = "nixpkgs=" + pkgs.path;
          PLAYWRIGHT_LAUNCH_OPTIONS_EXECUTABLE_PATH ="${pkgs.playwright-driver.browsers}/chromium-1091/chrome-linux/chrome";
          PLAYWRIGHT_BROWSERS_PATH="${pkgs.playwright-driver.browsers}";
          PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS="true";
        };
      });
    };
}
