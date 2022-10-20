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
        "aeson",
        "base",
        "bytestring",
        "cmark",
        "http-types",
        "scotty",
        "streaming-commons",
        "text",
        "unordered-containers",
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

# Node.js (for Elm)

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "c29944ba9b0b430aadcaf3bf2570fece6fc5ebfb76df145c6cdad40d65c20811",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/5.7.0/rules_nodejs-5.7.0.tar.gz"],
)

load("@build_bazel_rules_nodejs//:index.bzl", "node_repositories", "yarn_install")

node_repositories(
    node_version = "16.18",
    yarn_version = "3.2.4",
)

yarn_install(
    name = "npm",
    package_json = "//:package.json",
    yarn_lock = "//:yarn.lock",
)

# Elm

local_repository(
    name = "rules_elm",
    path = "tools/rules_elm",
)

load("@rules_elm//elm:deps.bzl", "elm_register_toolchains")

elm_register_toolchains()

load("@rules_elm//repository:def.bzl", "elm_repository")

elm_repository(
    name = "elm_package_elm_browser",
    sha256 = "a3001a00222e0a7e251bf0b622119384110e8ffac7e9a3c1142d382cf5d60325",
    strip_prefix = "browser-1.0.2",
    urls = ["https://github.com/elm/browser/archive/1.0.2.zip"],
)

elm_repository(
    name = "elm_package_elm_core",
    sha256 = "1ba2e027ab58f0ed41eea196fc4b016d6f5005926a9eecb1a3dffb4b2e213522",
    strip_prefix = "core-1.0.5",
    urls = ["https://github.com/elm/core/archive/1.0.5.zip"],
)

elm_repository(
    name = "elm_package_mdgriffith_elm_ui",
    sha256 = "b83b46bc62d44e9c8e01b20bb6b076fa0ee16ac029fe86120316ee0884ed5dee",
    strip_prefix = "elm-ui-1.1.8",
    urls = ["https://github.com/mdgriffith/elm-ui/archive/refs/tags/1.1.8.zip"],
)

elm_repository(
    name = "elm_package_elm_html",
    sha256 = "3fc68c94637fa6fefad671d93527857851533186a069f53cdc48a548dd26b546",
    strip_prefix = "html-1.0.0",
    urls = ["https://github.com/elm/html/archive/1.0.0.zip"],
)

elm_repository(
    name = "elm_package_elm_virtual_dom",
    sha256 = "0158b7a6923b2692e6aa77600b45c80aaf1573f0a11aee24a36f7d30d6549186",
    strip_prefix = "virtual-dom-1.0.2",
    urls = ["https://github.com/elm/virtual-dom/archive/1.0.2.zip"],
)

elm_repository(
    name = "elm_package_elm_http",
    sha256 = "e5b58162c5e29ab9b5f7b132d4ae03309012efbac570d89eec9717189e769fd6",
    strip_prefix = "http-2.0.0",
    urls = ["https://github.com/elm/http/archive/2.0.0.zip"],
)

elm_repository(
    name = "elm_package_elm_json",
    sha256 = "c8f3e274e3b45900f3b71c5107f974266daf29ba1303c35e6f9d7e4f67e7b54b",
    strip_prefix = "json-1.1.3",
    urls = ["https://github.com/elm/json/archive/1.1.3.zip"],
)

elm_repository(
    name = "elm_package_elm_url",
    sha256 = "289b8e8e07775046cee897884433920f2ebc0b2bf0f0b6b93dc44111b39eea64",
    strip_prefix = "url-1.0.0",
    urls = ["https://github.com/elm/url/archive/1.0.0.zip"],
)

elm_repository(
    name = "elm_package_elm_bytes",
    sha256 = "08d55273137283ebc45a500c88e62ebdb0b217acce81782651b4e2d7602ca6c9",
    strip_prefix = "bytes-1.0.8",
    urls = ["https://github.com/elm/bytes/archive/1.0.8.zip"],
)

elm_repository(
    name = "elm_package_elm_file",
    sha256 = "73e1c9dc865e80b504f5dceeecea022f2d7766afa519edb68ebda758cb133d85",
    strip_prefix = "file-1.0.5",
    urls = ["https://github.com/elm/file/archive/1.0.5.zip"],
)

elm_repository(
    name = "elm_package_elm_regex",
    sha256 = "37737b8e27e113e6c8f7b37a178edcfcfebb7dca6276d3b662d39ff757464be1",
    strip_prefix = "regex-1.0.0",
    urls = ["https://github.com/elm/regex/archive/1.0.0.zip"],
)

elm_repository(
    name = "elm_package_elm_time",
    sha256 = "60923a5c94fbbafee44c6520ffe030fdbd2e727ddcdea42cb688d870f76ed36d",
    strip_prefix = "time-1.0.0",
    urls = ["https://github.com/elm/time/archive/1.0.0.zip"],
)

elm_repository(
    name = "elm_package_krisajenkins_remotedata",
    sha256 = "2d425a5574be26feeab8796513aa004f3a642a90cdb8524fd3b885c4111effcc",
    strip_prefix = "remotedata-6.0.1",
    urls = ["https://github.com/krisajenkins/remotedata/archive/refs/tags/6.0.1.zip"],
)

elm_repository(
    name = "elm_package_dillonkearns_elm_markdown",
    sha256 = "e85d4d59b72d4a08f0f46702c50f918e401f81fda066d8ea833892f95189f70b",
    strip_prefix = "elm-markdown-7.0.0",
    urls = ["https://github.com/dillonkearns/elm-markdown/archive/refs/tags/7.0.0.zip"],
)

elm_repository(
    name = "elm_package_rtfeldman_elm_hex",
    sha256 = "20ef30de152a850f01680b52c60b39c50909cb0a26b303780bdd596a656b032e",
    strip_prefix = "elm-hex-1.0.0",
    urls = ["https://github.com/rtfeldman/elm-hex/archive/refs/tags/1.0.0.zip"],
)

elm_repository(
    name = "elm_package_elm_parser",
    sha256 = "68c7c251470580561531b1076ffea80f1d9cc610c1af217008c8753e6d7b43a5",
    strip_prefix = "parser-1.1.0",
    urls = ["https://github.com/elm/parser/archive/refs/tags/1.1.0.zip"],
)

# Nginx

container_pull(
    name = "nginx_base",
    digest = "sha256:f0219f99a56ec88f02f1d6055e5b4565cb80dba10b2301aa18f47913456accac",
    registry = "index.docker.io",
    repository = "nginx",
    tag = "1.23.1-alpine",
)
