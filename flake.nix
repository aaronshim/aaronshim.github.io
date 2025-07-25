{
  description = "hakyll-nix-template";

  nixConfig = {
    allow-import-from-derivation = "true";
    bash-prompt = "[hakyll-nix]λ ";
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            hakyllProject = final.haskell-nix.project' {
              src = ./ssg;
              compiler-nix-name = "ghc910";
              modules = [{ doHaddock = false; }];
              shell.buildInputs = [
                hakyll-site
                final.git
              ];
              shell.tools = {
                cabal = "latest";
                hlint = "latest";
                haskell-language-server = "latest";
              };
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit overlays system;
          inherit (haskellNix) config;
        };
        
        # Get the GHC version from the actual compiler being used
        # The project uses compiler-nix-name = "ghc910" but that maps to ghc9101 internally
        hakyllProject = pkgs.hakyllProject;
        ghcVersion = hakyllProject.pkg-set.config.ghc.package.version;
        
        # Get the nixpkgs revision from the haskellNix input
        nixpkgsRev = haskellNix.inputs.nixpkgs-unstable.rev;

        flake = pkgs.hakyllProject.flake {};

        executable = "ssg:exe:hakyll-site";
        testCompName = "ssg:test:csp-test";
        ghcTestCompName = "ssg:test:ghc-version-resolver-test";

        hakyll-site = flake.packages.${executable};

        website = pkgs.stdenv.mkDerivation {
          name = "website";
          buildInputs = [];
          src = pkgs.nix-gitignore.gitignoreSourcePure [
            ./.gitignore
            ".git"
            ".github"
          ] ./.;

          # LANG and LOCALE_ARCHIVE are fixes pulled from the community:
          #   https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
          #   https://github.com/NixOS/nix/issues/318#issuecomment-52986702
          #   https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = if (pkgs.stdenv.isLinux && pkgs.buildPlatform.libc == "glibc")
            then "${pkgs.glibcLocales}/lib/locale/locale-archive"
            else "";

          # Pass GHC version information directly from Nix derivations
          GHC_VERSION = "${ghcVersion}";
          NIXPKGS_REV = "${nixpkgsRev}";

          buildPhase = ''
            echo "Starting website build process..."
            echo "GHC Version: $GHC_VERSION"
            echo "Nixpkgs Revision: $NIXPKGS_REV"
            
            echo "Running hakyll-site build..."
            ${flake.packages.${executable}}/bin/hakyll-site build --verbose
          '';

          installPhase = ''
            mkdir -p "$out/dist"
            cp -a dist/. "$out/dist"
          '';
        };

      in (builtins.removeAttrs flake ["hydraJobs"]) // (if system == "x86_64-linux" then { inherit ghcVersion; } else {}) // rec {
        
        apps = {
          default = flake-utils.lib.mkApp {
            drv = hakyll-site;
            exePath = "/bin/hakyll-site";
          };
        };

        packages = {
          inherit hakyll-site website;
          default = website;
        };

        checks = {
           # Individual test suites (run in parallel by nix) - system-specific
           csp-test = pkgs.runCommand "csp-test-runner" {
             buildInputs = [ flake.packages.${testCompName} ];
           } ''
             echo "Running CSP tests..."
             ${flake.packages.${testCompName}}/bin/csp-test
             echo "CSP tests completed successfully"
             touch $out
           '';
           
           ghc-version-resolver-test = pkgs.runCommand "ghc-version-resolver-test-runner" {
             buildInputs = [ flake.packages.${ghcTestCompName} ];
           } ''
             echo "Running GHC version resolver tests..."
             ${flake.packages.${ghcTestCompName}}/bin/ghc-version-resolver-test
             echo "GHC version resolver tests completed successfully"
             touch $out
           '';
           
           # Combined test that runs all and continues on failure
           all-tests = pkgs.runCommand "all-tests-runner" {
             buildInputs = [ 
               flake.packages.${testCompName} 
               flake.packages.${ghcTestCompName}
             ];
           } ''
             echo "Running all test suites with failure tolerance..."
             
             EXIT_CODE=0
             
             echo "Running CSP tests..."
             if ${flake.packages.${testCompName}}/bin/csp-test; then
               echo "✓ CSP tests completed successfully"
             else
               echo "✗ CSP tests failed"
               EXIT_CODE=1
             fi
             
             echo "Running GHC version resolver tests..."
             if ${flake.packages.${ghcTestCompName}}/bin/ghc-version-resolver-test; then
               echo "✓ GHC version resolver tests completed successfully"
             else
               echo "✗ GHC version resolver tests failed"
               EXIT_CODE=1
             fi
             
             if [ $EXIT_CODE -eq 0 ]; then
               echo "✓ All tests completed successfully"
             else
               echo "✗ Some tests failed, but all were executed"
               exit $EXIT_CODE
             fi
             
             touch $out
           '';
           
# End-to-end integration test using NixOS VM (Linux only)
        } // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
           e2e-test = pkgs.testers.runNixOSTest {
             name = "hakyll-site-e2e";
             
             nodes = {
               server = { config, pkgs, ... }: {
                 # Serve the built website
                 services.nginx = {
                   enable = true;
                   virtualHosts."localhost" = {
                     root = "${website}/dist";
                     locations."/" = {
                       index = "index.html";
                     };
                   };
                 };
                 
                 # Install Deno and Chromium for testing
                 environment.systemPackages = with pkgs; [ 
                   deno
                   chromium
                 ];
                 
                 # Copy test files
                 environment.etc."test/e2e/import_map.json".source = ./test/e2e/import_map.json;
                 environment.etc."test/e2e/site.spec.ts".source = ./test/e2e/site.spec.ts;
                 
                 # Allow nginx to bind to port 80
                 networking.firewall.allowedTCPPorts = [ 80 ];
               };
             };
             
             testScript = ''
               # Start the server and wait for nginx
               server.start()
               server.wait_for_unit("nginx.service")
               server.wait_for_open_port(80)
               
               # Wait a bit for nginx to be fully ready
               server.sleep(2)
               
               # Test that nginx is serving our site
               server.succeed("curl -f http://localhost/ > /tmp/index.html")
               server.succeed("grep -i 'my site name' /tmp/index.html")
               
               # Test that basic functionality works
               server.succeed("curl -f http://localhost/rss.xml")
               server.succeed("curl -f http://localhost/sitemap.xml")
               
               # TODO: Add Deno e2e tests once VM has internet access
             '';
           };
        };
      }
    );
}
