## Prerequisites

1. You should be logged into Docker Container Registry, i.e. you should have a `~/.docker/config.json` with your credentials.

```bash
docker login
```

### Developing in Haskell

**Updating dependencies**: add a dependency to `stack_snapshot` WORKSPACE rule, and then do `bazel run @stackage-unpinned//:pin`.

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