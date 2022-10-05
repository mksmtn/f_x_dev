workspace(name = "f_x_dev")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

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
        "scotty",
    ],
    snapshot = "lts-18.27",
    stack_snapshot_json = "//:stackage_snapshot.json",
)
