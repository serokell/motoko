{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.19/nix/sources.nix;
    sha256 = "1n92ka2rkdiib6ian6jh2b7fwvklnnwlp5yy5bv6ywm7m1y5hyfl";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  bootstrap-pkgs = import nixpkgs_src {
    system = builtins.currentSystem;
  };

  # dump nixpkgs patches here
  nixpkgs-patches = [ ];

  nixpkgs-patched =
    if nixpkgs-patches == []
    then nixpkgs_src
    else
      let
        bootstrap-pkgs = import nixpkgs_src {
          system = builtins.currentSystem;
        };
      in bootstrap-pkgs.applyPatches {
        name = "nixpkgs-patched";
        src = nixpkgs_src;
        patches = nixpkgs-patches;
      };

  pkgs =
    import nixpkgs-patched {
      inherit system;
      overlays = [
        # add nix/sources.json
        (self: super: {
           sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };
        })

        # Selecting the ocaml version
        # Also update ocaml-version in src/*/.ocamlformat!
        (self: super: { ocamlPackages = self.ocaml-ng.ocamlPackages_4_14; })

        (self: super: {
            # Additional ocaml package
            ocamlPackages = super.ocamlPackages // rec {

              # downgrade wasm until we have support for 2.0.1
              # (https://github.com/dfinity/motoko/pull/3364)
              wasm_1 = super.ocamlPackages.wasm.overrideAttrs rec {
                version = "1.1.1";
                src = self.fetchFromGitHub {
                  owner = "WebAssembly";
                  repo = "spec";
                  rev = "opam-${version}";
                  sha256 = "1kp72yv4k176i94np0m09g10cviqp2pnpm7jmiq6ik7fmmbknk7c";
                };
                patchPhase = ''
                  substituteInPlace ./interpreter/Makefile \
                    --replace-fail "+a-4-27-42-44-45" "+a-4-27-42-44-45-70"
                '';
              };

              ocaml-recovery-parser = super.ocamlPackages.buildDunePackage {
                pname = "ocaml-recovery-parser";
                version = "0.3.0";
                src = self.fetchFromGitHub {
                  owner = "serokell";
                  repo = "ocaml-recovery-parser";
                  rev = "b8207b0c919b84d5096486e59985d0137c0c4d82";
                  sha256 = "1xp88i26vlwjsvnlzbfvk7zx31s18pfwq9w4g2fb2xq7bbnlhm24";
                };
                buildInputs = with super.ocamlPackages; [
                  menhirSdk
                  menhirLib
                  fix
                  base
                ];
              };

              # No testing of atdgen, as it pulls in python stuff, tricky on musl
              atdgen = super.ocamlPackages.atdgen.overrideAttrs { doCheck = false; };
            };
          }
        )

        # Mozilla overlay
        (self: super:
          { moz_overlay = import self.sources.nixpkgs-mozilla self super; }
        )

        # Rust nightly
        (self: super: let
          rust-channel = self.moz_overlay.rustChannelOf { date = "2024-07-28"; channel = "nightly"; };
        in rec {
          rustc-nightly = rust-channel.rust.override {
            targets = [
               "wasm32-wasi"
            ];
            extensions = ["rust-src"];
          };
          cargo-nightly = rustc-nightly;
          rustPlatform-nightly = self.makeRustPlatform {
            rustc = rustc-nightly;
            cargo = cargo-nightly;
          };
        })

        # Rust stable
        (self: super: let
          rust-channel = self.moz_overlay.rustChannelOf { version = "1.85.0"; channel = "stable"; };
        in {
          rustPlatform_moz_stable = self.makeRustPlatform rec {
            rustc = rust-channel.rust;
            cargo = rustc;
          };
        })

        # wasm-profiler
        (self: super: import ./wasm-profiler.nix self)

        # drun
        (self: super: import ./drun.nix self)

        # to allow picking up more recent Haskell packages from Hackage
        # don't use `fetchFromGitHub` here as we really need an intact tarball
        (self: super: {
          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/d859530d8342c52d09a73d1d125c144725b5945d.tar.gz";
            sha256 = "0gjahsqqq99dc4bjcx9p3z8adpwy51w3mzrf57nib856jlvlfmv5";
          };
        })
      ];
    };
in
pkgs
