## Developer onboarding

1. Install `bazelisk`.

2. Install `buildifier`.

```bash
cd tools/buildtools

bazelisk build //buildifier
```

3. Install the recommended extentions.

4. Setup Bazel extentions. Go to Bazel extention settings and:

   1. Add path to `buildifier` executable. On my machine it's `/home/mksmtn/Coding/f_x_dev/tools/buildtools/bazel-bin/buildifier/buildifier_/buildifier`.

   2. Add path to `bazel` executable, which should be `bazelisk`.

5. Install the dependencies required by [rules_haskell](https://rules-haskell.readthedocs.io/en/latest/haskell.html)

```bash
apt-get install build-essential libffi-dev libgmp-dev libtinfo5 libtinfo-dev python python3 openjdk-11-jdk
```

6. Install git hooks
