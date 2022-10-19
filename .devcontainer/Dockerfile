FROM haskell:8.10.7

# Install Bazel
RUN curl -L https://github.com/bazelbuild/bazel/releases/download/5.2.0/bazel-5.2.0-linux-x86_64 -o /usr/local/bin/bazel
RUN chmod +x /usr/local/bin/bazel
# $(cat .bazelversion)

# Install Buildifier
COPY tools/ tools/
WORKDIR tools/buildtools
RUN bazel build //buildifier
RUN mv bazel-bin/buildifier/buildifier_/buildifier /usr/local/bin/buildifier
WORKDIR /workspaces/f_x_dev

# Install rules_haskell dependencies
RUN apt update && apt-get install --assume-yes build-essential libffi-dev libgmp-dev libtinfo5 libtinfo-dev python python3 openjdk-11-jdk

# Set up Docker authentication for rules_docker
ENV DOCKER_CONFIG /etc/docker

# Install kubectl
RUN curl -L https://dl.k8s.io/release/v1.24.6/bin/linux/amd64/kubectl -o /usr/local/bin/kubectl
RUN chmod +x /usr/local/bin/kubectl

# Set up authentication for k8s cluster in the cloud
ENV KUBECONFIG /workspaces/f_x_dev/secrets/config.yaml

# Install Helm
RUN curl -L https://get.helm.sh/helm-v3.10.1-linux-amd64.tar.gz -o /tmp/helm.tar.gz
RUN tar -xzf /tmp/helm.tar.gz -C /tmp
RUN mv /tmp/linux-amd64/helm /usr/local/bin/helm

# Install Elm
RUN curl -L https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz -o /tmp/elm.gz
RUN gunzip /tmp/elm.gz
RUN mv /tmp/elm /usr/local/bin/elm
RUN chmod +x /usr/local/bin/elm

ENTRYPOINT ["/bin/bash"]
