# Contributing

Please note we have a [code of conduct](https://github.com/notquiteamonad/tie/blob/main/CODE_OF_CONDUCT.md). Please follow it in all your interactions with the project.

## Issues and Feedback Process

- If you spot any issues with the project or wish to request any new features, feel free to add an issue in the GitHub issue tracker.
- You can also use the issue tracker to leave feedback which I'll respond to if required.

## Pull Request Process

1. Pick up any of our issues in the issue tracker (feel free to self-assign these).
2. If you have any questions, feel free to ask on the issue.
3. Once you've edited the repo, put in a PR. Assign [@notquiteamonad](https://github.com/notquiteamonad/) and attach any labels you think fit the PR. Bots may add more labels once you submit the PR.
4. I'll get back to you with any changes that need making before the PR can be merged, and merge it once it all looks good.

## Coding Standards

Code in this repository is held to a high standard, but don't let that put you off from contributing; it's all about following these simple rules:

- Code submitted should be formatted using stylish-haskell.
- Code submitted should pass the hlint rules defined in `.hlint.yaml`.
- All code submitted should be considered for being covered by tests.

These good practices are enforced by the following procedures:

- A pre-commit hook can check these things when you make a commit. This can be enabled by running `./scripts/install_hooks.sh`.
- Continuous Integration via GitHub Actions is used to check that code is formatted correctly and works as expected.
- All PRs will be reviewed by a maintainer. Here, suggestions will be made on how to improve the code until it is agreed that it is of the required standard.
