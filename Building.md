## Nix setup

The Motoko build system relies on [Nix](https://nixos.org/) to manage
dependencies, drive the build and run the test suite. You should install `nix` by
running, as a normal user with `sudo` permissions,
```
sh <(curl -L https://nixos.org/nix/install) --daemon
```

This repository is also a Nix Flake which means you need to
allow this feature by making sure the following is present in `/etc/nix/nix.conf`:
```
extra-experimental-features = nix-command flakes
```

You should also enable a nix cache to get all dependencies pre-built.

The `cachix` command also requires `sudo` permissions.
```
nix profile install --accept-flake-config nixpkgs#cachix
cachix use ic-hs-test
```
Technically, this is optional, but without this you will build lots of build
dependencies manually, which can take several hours.

## Installation using Nix

If you want just to _use_ `moc`, you can install the `moc` binary into your `nix`
environment by running
```
$ nix profile install .#release.moc
```
in a check-out of the `motoko` repository.

## Development using Nix

To enter a shell with the necessary dependencies available,
either run:

```
$ nix develop
```

Or use `direnv` by:

* Installing: [direnv](https://direnv.net/).

* Installing: [nix-direnv](https://github.com/nix-community/nix-direnv).

* `cd` to this directory.

* `direnv allow` (only needs to be done once).

Then all tools to develop Motoko will be loaded automatically everytime you `cd`
to this directory or everytime you update `flake.{nix,lock}`.

(The first shell start may take several minutes, afterwards being much faster.)

Within this shell you can run
 * `make` in `src/` to build all binaries,
 * `make moc` in `src/` to build just the `moc` binary,
 * `make DUNE_OPTS=--watch moc` to keep rebuilding as source files are changing
 * `make` in `rts/` to build the Motoko runtime
 * `make` in `test/` to run the test suite.

This invokes `dune` under the hood, which will, as a side effect, also create
`.merlin` files for integration with Merlin, the Ocaml Language Server

## Replicating CI locally

A good way to check that everything is fine, i.e. if this will pass CI, is to run
```
$ nix build --no-link
```

For more details on our CI and CI setup, see `CI.md`.


## Making releases

We make frequent releases, at least weekly. The steps to make a release (say, version 0.14.1) are:

 * Make sure that the top section of `Changelog.md` has a title like

        ## 0.14.1 (2025-02-10)

   with today’s date.

 * Make sure the markdown doc for base is up-to-date:
   For now, in a nix shell (preferably _re-entering_):

   ```bash
      make -C rts
      make -C src
      make -C doc base
      git diff
   ```

   If not, create and merge a separate PR to update the doc (adding any new files) and goto step 0.

 * Define a shell variable `MOC_MINOR` with the next minor version number. E.g. `export MOC_MINOR=1`

 * Look at `git log --first-parent 0.14.$(expr $MOC_MINOR - 1)..HEAD` and check
   that everything relevant is mentioned in the changelog section, and possibly
   clean it up a bit, curating the information for the target audience.

 * `git commit -am "chore: Releasing 0.14."$MOC_MINOR`
 * Create a PR from this commit, and label it `automerge-squash`. E.g.
   with `git push origin HEAD:$USER/0.14.$MOC_MINOR`. Mergify will
   merge it into `master` without additional approval, but it will take some
   time as the title (version number) enters into the `nix` dependency tracking.
 * `git switch master; git pull --rebase`. The release commit should be your `HEAD`
 * `git show` to verify we are at the right commit
 * `git tag 0.14.$MOC_MINOR -m "Motoko 0.14."$MOC_MINOR`
 * `git push origin 0.14.$MOC_MINOR`

Pushing the tag should cause GitHub Actions to create a “Release” on the GitHub
project. This will fail if the changelog is not in order (in this case, fix and
force-push the tag).  It will also fail if the nix cache did not yet contain
the build artifacts for this revision. In this case, restart the GitHub Action
on GitHub’s UI.

After releasing the compiler you can update `motoko-base`'s `master`
branch to the `next-moc` branch.

* Wait ca. 5min after releasing to give the CI/CD pipeline time to upload the release artifacts
* Change into `motoko-base`
* `git switch next-moc; git pull`
* `git switch -c $USER/update-moc-0.14.$MOC_MINOR`
* Revise and update the `CHANGELOG.md`, by adding a top entry for the release
* Update the `moc_version` env variable in `.github/workflows/{ci, package-set}.yml` and `mops.toml`
  to the new released version:
  `perl -pi -e "s/moc_version: \"0\.14\.\\d+\"/moc_version: \"0.14.$MOC_MINOR\"/g; s/moc = \"0\.14\.\\d+\"/moc = \"0.14.$MOC_MINOR\"/g; s/version = \"0\.14\.\\d+\"/version = \"0.14.$MOC_MINOR\"/g" .github/workflows/ci.yml .github/workflows/package-set.yml mops.toml`
* `git add .github/ CHANGELOG.md mops.toml && git commit -m "Motoko 0.14."$MOC_MINOR`
* You can `git push` now

Make a PR off of that branch and merge it using a _normal merge_ (not
squash merge) once CI passes. It will eventually be imported into this
repo by a scheduled `niv-updater-action`.

Finally tag the base release (so the documentation interpreter can do the right thing):
* `git switch master && git pull`
* `git show` to verify we are at the right commit
* `git tag moc-0.14.$MOC_MINOR`
* `git push origin moc-0.14.$MOC_MINOR`

### Downstream

There are a few dependent actions to follow-up the release, e.g.
- `motoko` NPM package
- `vessel` package set
- `vscode` plugin
- ICP Ninja

These are generally triggered by mentioning the release in Slack.

Announcing the release towards SDK happens by triggering this GitHub action:
https://github.com/dfinity/sdk/actions/workflows/update-motoko.yml
Press the "Run workflow" button, filling in
- Motoko version: `latest`
- Open PR against this sdk branch: `master`

and then hitting the green button. This will create a PR with all necessary hash changes against that branch. There is no
need to do this immediately, you can leave the release soaking a few days. Use your own jugdement w.r.t. risk, urgency etc.

If you want to update the portal documentation, typically to keep in sync with a `dfx` release, follow the instructions in https://github.com/dfinity/portal/blob/master/MAINTENANCE.md.

## Coverage report

To build with coverage enabled, compile the binaries in `src/` with
```
make DUNE_OPTS="--instrument-with bisect_ppx"`
```
and then use `bisect-ppx-report html` to produce a report.

The full report can be built with
```
nix build .#tests.coverage
```
and the report for latest `master` can be viewed at
[https://dfinity.github.io/motoko/coverage/](https://dfinity.github.io/motoko/coverage/).

## Profile the compiler

(This section is currently defunct, and needs to be update to work with the dune
build system.)

1. Build with profiling within nix-shell (TODO: How to do with dune)
   ```
   make -C src clean
   make BUILD=p.native -C src moc
   ```
2. Run `moc` as normal, e.g.
   ```
   moc -g -c foo.mo -o foo.wasm
   ```
   this should dump a `gmon.out` file in the current directory.
3. Create the report, e.g. using
   ```
   gprof --graph src/moc
   ```
   (Note that you have to _run_ this in the directory with `gmon.out`, but
   _pass_ it the path to the binary.)


## Benchmarking the RTS

Specifically some advanced techniques to obtain performance deltas for the
GC can be found in `rts/Benchmarking.md`.
