workspace(name = "f_x_dev")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

# Haskell

http_archive(
    name = "rules_haskell",
    sha256 = "aba3c16015a2363b16e2f867bdc5c792fa71c68cb97d8fe95fddc41e409d6ba8",
    strip_prefix = "rules_haskell-0.15",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.15.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

rules_haskell_toolchains(
    version = "8.10.7",
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        "base",
        "cmark",
        "http-types",
        "scotty",
        "streaming-commons",
        "text",
        "wai-middleware-static",
        "warp",
    ],
    snapshot = "lts-18.27",
    stack_snapshot_json = "//:stackage_snapshot.json",
)

# Docker

http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "b1e80761a8a8243d03ebca8845e9cc1ba6c82ce7c5179ce2b295cd36f7e394bf",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.25.0/rules_docker-v0.25.0.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)

container_repositories()

load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")

container_deps()

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)

container_pull(
    name = "ubuntu_base",
    digest = "sha256:a8fe6fd30333dc60fc5306982a7c51385c2091af1e0ee887166b40a905691fd0",
    registry = "index.docker.io",
    repository = "ubuntu",
)

container_pull(
    name = "haskell_base",
    digest = "sha256:c38804b87bd64dce802c1a3fc5bd1021069b820b27612ac2d1e3e0396f5ad49c",
    registry = "index.docker.io",
    repository = "haskell",
)

# Kubernetes

http_file(
    name = "k8s_binary",
    downloaded_file_path = "kubectl",
    executable = True,
    sha256 = "b150c7c4830cc3be4bedd8998bf36a92975c95cd1967b4ef2d1edda080ffe5d9",
    urls = ["https://dl.k8s.io/release/v1.23.12/bin/linux/amd64/kubectl"],
)

http_archive(
    name = "io_bazel_rules_k8s",
    sha256 = "773aa45f2421a66c8aa651b8cecb8ea51db91799a405bd7b913d77052ac7261a",
    strip_prefix = "rules_k8s-0.5",
    urls = ["https://github.com/bazelbuild/rules_k8s/archive/v0.5.tar.gz"],
)

load("@io_bazel_rules_k8s//toolchains/kubectl:kubectl_configure.bzl", "kubectl_configure")

kubectl_configure(
    name = "k8s_config",
    kubectl_path = "@k8s_binary//file",
)

load("@io_bazel_rules_k8s//k8s:k8s.bzl", "k8s_repositories")

k8s_repositories()

load("@io_bazel_rules_k8s//k8s:k8s_go_deps.bzl", k8s_go_deps = "deps")

k8s_go_deps()
