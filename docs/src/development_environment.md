# Development environment

## Developer shell

To enter the developer shell, run:

```sh
nix develop
```

To automatically enter the developer shell upon entering the project directory
with [`direnv`](https://direnv.net), run:

```sh
direnv allow
```

## `pre-commit`

The default developer shell leverages [`pre-commit`](https://pre-commit.com)
hooks to simplify the process of reaching minimum quality standards for casual
contributors. This means applying code formatters, and scanning for things like
unused variables which should be removed.

By default, once you have entered the developer shell, `pre-commit` runs
automatically just before you create a commit. This will only look at the
files which are about to be committed.

You can also run it manually against all files:

```sh
pre-commit run --all-files
```

This is useful if a commit was created outside of the developer shell, and
you need to apply `pre-commit` to your previous changes.

Note that there is also a flake output, `.#checks.«system».git-hooks`, which
always runs against all files but does not have access to apply changes. This
is used in GitHub Actions to ensure that `pre-commit` has been applied.

## `stylix-check`

When a pull request is opened, we use GitHub Actions to build everything under
`.#checks`. This includes the previously mentioned `.#checks.«system».git-hooks`,
and every [testbed](./testbeds.md).

You might sometimes find it useful to run these same checks locally. The built
in `nix flake check` command does this, however it can be quite slow compared
to the script we use on GitHub Actions.

To use the same script that we use, you can run this command within the
developer shell:

```sh
stylix-check
```

This is based on [`nix-fast-build`](https://github.com/Mic92/nix-fast-build#readme).
