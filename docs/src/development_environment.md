# Development environment

## Developer shell

To enter the developer shell, run:

```console
nix develop
```

To automatically enter the developer shell upon entering the project directory
with [`direnv`](https://direnv.net), run:

```console
direnv allow
```

## pre-commit

The default developer shell leverages [`pre-commit`](https://pre-commit.com)
hooks to simplify the process of reaching minimum quality standards for casual
contributors.

By default, `pre-commit` only runs on staged files. To manually run
[`pre-commit`](https://pre-commit.com) against all files, run:

```console
pre-commit run --all-files
```

This is useful when submitting a patchset and `pre-commit` was not used on all
commits. For example, suppose the first commit was created without `pre-commit`
and touches `/flake.nix`. Installing `pre-commit` and then creating a second
commit that touches `/README.md` will not run any hooks on `/flake.nix`.

Note that the `outputs.checks.${system}.git-hooks` output always runs against
all files.
