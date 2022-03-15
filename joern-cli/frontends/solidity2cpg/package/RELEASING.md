# Releasing

The following sections describe this repo's release methodology. Some of it is automated in Travis CI, some of it should be performed manually by this repo's maintainers.

## Prerelases

Every commit to the `develop` branch increments the prerelease version of the Surya package with the following command:

```npm version prerelease -m "chore: release version %s [skip ci]"```

which is a part of the `.travis/prerelease_pu.sh` script. The script bumps up the version and commits to GitHub without triggering a Travis build again.

No action is need from maintainers to trigger a prerelease publish.

## Releases

Travis CI is also configured to publish the package to NPM on every **tagged** commit to `master`.

What this means is that maintainers should bump the version themselves (should not be a prerelease) by any mean they wish after every PR from `develop` and therefore trigger a publish in Travis.

Following the SemVer specification, if every other process before was followed, you should be able to only delete the *label* part of the SemVer string.

i.e.: `v1.2.3-dev.4` would be changed to `v1.2.3`

Suggested methods are to either run the following commands on a clean git directory (which can be checked with `git clean -n`):

```
npm version [major | minor | patch] -m "chore: release version %s"
git push --follow-tags origin master
```

or, if increasing the version manually:

```
git add package.json package-lock.json
git commit -m "chore: release version vX.Y.Z"
git tag -a vX.Y.Z -m "chore: release version vX.Y.Z"
git push --follow-tags origin master
```

After this, the a commit merging the `master` branch with the `develop` one should be made.

e.g.: if the `master` branch version was changed to `v1.2.3`, then the new `develop` branch version to be committed should read `v1.2.3`, too. Travis will automatically increase it to the new `v1.2.4-dev.0`.