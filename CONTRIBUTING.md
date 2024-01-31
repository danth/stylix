# Contributing to Stylix

## Commit messages

To keep things consistent, commit messages should follow a format
[similar to Nixpkgs](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md#commit-conventions):

```
«scope»: «summary»

«motivation for change»
```

Where the scope is one of:

| Scope          | Purpose                                                                |
|----------------|------------------------------------------------------------------------|
| `doc`          | Changes to the website, `README.md`, and so on.                        |
| `stylix`       | Changes in the `stylix` directory, `flake.nix`, and other global code. |
| Name of target | Changes to code for a particular target.                               |
| `treewide`     | Changes across many targets.                                           |

The scope is meant to indicate which area of the code was changed. Specifying
the type of change, such as `feat` or `fix`, is not necessary. Dependency
updates should use whichever scope they are related to.

The summary should start with a lowercase letter, and should not end with
punctuation.

Most commits to `master` will also include a pull request number in brackets
after the summary. GitHub adds this automatically when creating a squash merge.

### Old commits

Commits before 2024 did not follow any particular format.
Some have emojis from [GitMoji](https://gitmoji.dev).
