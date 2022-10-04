## Developer onboarding

1. Install `bazelisk`

2. Install `buildifier`

3. Install the dependencies required by [rules_haskell](https://rules-haskell.readthedocs.io/en/latest/haskell.html)

```bash
apt-get install build-essential libffi-dev libgmp-dev libtinfo5 libtinfo-dev python python3 openjdk-11-jdk
```

4. Install the recommended extentions

5. Run`Buildifier` executable on file changes `bazelisk run //:buildifier -- --lint=warn`

6. Install git hooks
