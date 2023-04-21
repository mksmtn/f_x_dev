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

**Updating dependencies**: add a dependency to `stack_snapshot` WORKSPACE rule, and then do `bazel run @stackage-unpinned//:pin`.

## Cons 

1. Необходимо оптимизировать докерфайл для прода

2. При обновлении докерфайла сбросится кэш (например у Bazel) и не сохраняется история в консоли

## TODO

1. Use private docker registry

2. Add linting and formatting

3. Add type hint and autocompletion


### Issuing a certificate

1. helm repo add jetstack https://charts.jetstack.io

2. helm repo update

3. kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.11.0/cert-manager.crds.yaml

4. helm install \
  cert-manager jetstack/cert-manager \
  --namespace cert-manager \
  --create-namespace \
  --version v1.11.0

5. kubectl create -f projects/blog/k8s/issuer.yaml --kubeconfig secrets/config.yaml

6. kubectl get certificate