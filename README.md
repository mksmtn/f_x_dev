## Prerequisites

1. You should be logged into Docker Container Registry, i.e. you should have a `~/.docker/config.json` with your credentials.

```bash
docker login
```

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

6. [Install GHC 8.10.7](https://www.haskell.org/ghcup/#)

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

7. [Install Stack](https://docs.haskellstack.org/en/stable/)

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

8. Download `kubectl` version 1.23.

```bash
curl -L https://dl.k8s.io/release/v1.23.12/bin/linux/amd64/kubectl -o bin/kubectl
```

9. Place `config.yaml` with authentication settings into `secrets/`

10. Install git hooks

### Developing in Haskell

**Updating dependencies**: add a dependency to `stack_snapshot` WORKSPACE rule, and then do `bazelisk run @stackage-unpinned//:pin`.
