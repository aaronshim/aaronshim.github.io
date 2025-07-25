# Static Site

## 🚨 Important Setup - haskell.nix Binary Cache

**This step is critical for local development!** Without the haskell.nix binary cache, Nix will attempt to build GHC from source, requiring 13GB+ disk space and hours of compilation.

📖 **Follow the official setup guide**: [haskell.nix Binary Cache Configuration](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache)

This configures pre-built GHC packages that make builds fast and practical for local development.

## 🧪 Development Commands

For **macOS** users:
```bash
nix flake check --accept-flake-config    # Run tests
```

For **Linux** users, you can also use:
```bash  
nix build .#checks.x86_64-linux.csp-test --accept-flake-config    # Run tests
nix flake check --accept-flake-config                             # Run tests (alternative)
```

## Origins

Started off with [a template](https://github.com/rpearce/hakyll-nix-template)
and its
[accompanying tutorial](https://robertwpearce.com/the-hakyll-nix-template-tutorial.html)
from [rpearce](https://github.com/rpearce).

It uses [Hakyll](https://jaspervdj.be/hakyll/) + [Nix](https://nixos.org).

## Changes

- Updated the Github Actions workflows to use `actions/deploy-pages@v4` instead
  (and changed Pages deployment settings to allow this).
- Added `actions/attest-build-provenance@v2` for SLSA attestations of the build.

## Previous Documentation

### Quick tips

- Read the tutorial to get started!
  https://robertwpearce.com/the-hakyll-nix-template-tutorial.html
- If you make changes to anything inside of `ssg/`, you'll need to clean the
  hakyll cache and rebuild. This is the preferred series of commands for
  rebuilding (with logs), cleaning the cache, and re-running the dev server:

  ```default
  nix build --print-build-logs && \
    nix run . clean && \
    nix run . watch
  ```

### Features

- Build your site into the `./result/dist` folder:
  ```
  λ nix build
  ```
- Start hakyll's dev server that reloads when changes are made:
  ```
  λ nix run . watch
  Listening on http://127.0.0.1:8000
  ...more logs
  ```
- Run any hakyll command through `nix run .`!
  ```
  λ nix run . clean
  Removing dist...
  Removing ssg/_cache...
  Removing ssg/_tmp...
  ```
- Start a development environment that

  - has your shell environment
  - has `hakyll-site` (for building/watching/cleaning hakyll projects)
  - has `hakyll-init` (for generating new projects)
  - can have anything else you put in the `shell.buildInputs` of the
    `hakyllProject` in `flake.nix`
  - is set up to run `ghci` with some defaults and the modules loaded so you can
    make your own changes and test them out in the ghci REPL

  ```
  λ nix develop

  [hakyll-nix]λ hakyll-site build
  ...
  Success

  [hakyll-nix]λ ghci
  ...
  [1 of 1] Compiling Main    ( ssg/src/Main.hs, interpreted )
  ...

  λ >
  ```

#### hakyll

All of this is custmomizable, and here are some things that are already done for
you:

- [pandoc](https://github.com/jgm/pandoc/) markdown customization to make it as
  close to GitHub's markdown style as possible
- [`slugger`](https://hackage.haskell.org/package/slugger) module is included
  that makes nice link URIs based on post titles
- RSS & Atom XML feed generation
- Sitemap generation
- Code syntax highlighting customization
- ...other reasonable defaults

Configure the dev server, cache & tmp directories, and more in
`./ssg/src/Main.hs`.

#### Deployment

Deployment is set up through a
[GitHub Action](https://github.com/features/actions) with
[cachix](https://cachix.org), and it deploys to a
[GitHub Pages](https://pages.github.com/) branch, `gh-pages`, when you merge
code into your main branch.

Setup information can be found below in the "Cachix" section.

Note: If your main branch's name isn't `main`, ensure `'refs/heads/main'` gets
updated to `'refs/heads/my-main-branch'` in `./github/workflows/main.yml`.

### Setup

#### Nix & Flakes

If you don't have [nix](https://nixos.org), follow
[the nix installation instructions](https://nixos.org/download.html).

Once you have nix installed, follow the instructions here to get access to
flakes: https://nixos.wiki/wiki/Flakes.

#### Cachix

The `./.github/workflows/main.yml` file builds with help from
[cachix](https://app.cachix.org), so you'll need to generate a signing key to be
able to do this.

1. Create a cache on cachix for your project
1. Follow cachix's instructions to generate a signing keypair
1. Copy the signing keypair value to a new `CACHIX_SIGNING_KEY` secret on
   https://github.com/settings/secrets
