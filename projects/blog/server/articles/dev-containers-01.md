# Контейнеры разработки: дев среда как код

> Кто ищет, тот всегда найдёт, но не всегда то, что искал.

Когда разрабатывал бэкэнд для блога, я столкнулся со следующей проблемой: бинарник, полученный после компиляции Haskell-кода на моей Ubuntu, не работал внутри Docker контейнера на Debian. Хотя, казалось бы, эти операционные системы очень похожи. Возможно, дело в версиях каких-то библиотек. Пытаясь решить проблему, я наткнулся на неожиданное решение. Оказывается, в Visual Studio Code есть [возможность](https://containers.dev/) разрабатывать внутри Docker контейнера. Это очень крутая фича, решающая целый пласт проблем. Я стал разрабатывать и компилировать код внутри контейнера с Debian, и ошибка прошла. Сегодня расскажу, что такое контейнеры разработки, как ими пользоваться и приведу пример их настройки.

Управление средой разработки — это боль. Я работал на проектах, где из рук в руки перекочёвывают скрипты для установки необходимых разработчику инструментов и настройки моков. И даже в простом проекте на Node.JS недавно столкнулись с проблемой: код, который я написал, не работал у другого разработчика. Оказалось, у него 14-ая версия Node.JS, поэтому не было метода `at` у массивов. Подобных проблем можно было бы избежать, если бы было средство, позволяющее всем разработчикам иметь идентичную среду. И такое средство есть: Docker образы. Но как в них разрабатывать? Оказывается, в VS Code есть официальное расширение, позволяющее разрабатывать внутри контейнера. При наличии в проекте файла `.devcontainer/devcontainer.json` с необходимым настройками, IDE предложит открыть проект внутри описанного контейнера. В итоге все инструменты, установленные внутри контейнера, становятся доступны, а изменения в коде автоматически синхронизируются между основной ОС и контейнером; разработчику нужно иметь на компе только Docker и VS Code. Не нужно устанавливать никакие компиляторы, менеджеры зависимостей, сборщики, форматтеры кода и т.д., всё это будет установлено с помощью Docker-образа.

![Внутри контейнера разработки](/assets/images/inside-dev-container.png)

## Пример f(x)

Когда начинал проект, я поддерживал в `README.md` инструкцию для настройки среды разработки. Выглядела она примерно так:

1. Установить `bazelisk`;

2. Установить `buildifier`:

```bash
cd tools/buildtools
bazelisk build //buildifier
```

3. Установить рекомендуемые расширения для VS Code;

4. Перейти в настройки расширения Bazel и добавить пути к `buildifier` и к `bazel`;

5. Установить нужные для [rules_haskell](https://rules-haskell.readthedocs.io/en/latest/haskell.html) зависимости:

```bash
apt-get install build-essential libffi-dev libgmp-dev libtinfo5 libtinfo-dev python python3 openjdk-11-jdk
```

6. Установить [GHC 8.10.7](https://www.haskell.org/ghcup/#):

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

7. Установить [Stack](https://docs.haskellstack.org/en/stable/):

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

8. Поставить `kubectl` версии 1.23:

```bash
curl -L https://dl.k8s.io/release/v1.23.12/bin/linux/amd64/kubectl -o bin/kubectl
```

9. Поместить файл `config.yaml` с авторизационными данными в `secrets/`.

Девять пунктов для такого маленького проекта! После перехода на Dev Containers инструкция сократилась до трёх пунктов:

1. Установить Docker CLI и залогиниться в нём;

```bash
docker login
```

2. Получить файл с секретами `secrets/config.yaml`;

3. Нажать кнопку во всплывающем окне VS Code для открытия проекта внутри контейнера.

А вся логика установки компиляторов, сборщиков, CLIs и прочего переехала в `.devcontainer/Dockerfile`:

```docker
FROM haskell:8.10.7

# Install Bazel
RUN curl -L https://github.com/bazelbuild/bazel/releases/download/5.2.0/bazel-5.2.0-linux-x86_64 -o /usr/local/bin/bazel
RUN chmod +x /usr/local/bin/bazel

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

# Install Elm
RUN curl -L https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz -o /tmp/elm.gz
RUN gunzip /tmp/elm.gz
RUN mv /tmp/elm /usr/local/bin/elm
RUN chmod +x /usr/local/bin/elm

ENTRYPOINT ["/bin/bash"]
```

Содержание `.devcontainer/devcontainer.json`:

```json
{
  "name": "Dev Container for f(x) monorepo",
  "build": {
    "context": "..",
    "dockerfile": "Dockerfile"
  },
  "mounts": ["source=${localEnv:HOME}/.docker/,target=/etc/docker/,type=bind,readonly=true,consistency=cached"],
  "extensions": [
    "bazelbuild.vscode-bazel",
    "haskell.haskell"
  ],
  "settings": {
    "files.exclude": {
      "**/.git": true,
      "**/.svn": true,
      "**/.hg": true,
      "**/CVS": true,
      "**/.DS_Store": true,
      "**/Thumbs.db": true,
      "**/bazel-*": true
    }
  }
}
```

Из интересного в `.devcontainer/devcontainer.json` разве что поле `mounts`. В нём прописано, что содержимое папки `~/.docker` основной ОС нужно смонтировать в папку контейнера `/etc/docker`. Таким образом авторизационные токены Docker Registry, настроенные для основной ОС, будут доступны внутри контейнера.

## Пошаговая инструкция для простого проекта на Python

1. Перейти на [сайт со списком готовых дев контейнеров](https://containers.dev/templates);

2. Выбрать Python 3;

3. Открыть проект в VS Code и скопировать файл `.devcontainer/devcontainer.json` в свой проект;

4. Поменять `${templateOption:imageVariant}` на нужную версию, например `3.10-bullseye`;

5. Удалить поле `features`, так как Node.JS не нужна;

6. Открыть список команд VS Code: `Shift+Ctrl+P`;

7. Выбрать `Dev Containers: Rebuild and Reopen in Container`;

![Собрать дев контейнер](/assets/images/rebuild-and-open-in-container.png)

8. Готово, вы восхитительны! Можно разрабатывать внутри контейнера и пользоваться всеми установленными в нём инструментами (в данном случае python 3.10). А другие разработчики смогут начать разрабатывать внутри контейнера, просто нажав одну кнопку во всплывающем окне.

![Открыть дев контейнер](/assets/images/open-in-container.png)

## Недостатки

На данный момент я столкнулся с одним недостатком: при обновлении Dockerfile (например после обновления компилятора в образе) все данные внутри старого контейнера удалятся. В моём случае такими данными являются кэш у Bazel и история в терминале. Поэтому первая сборка проекта после обновления образа занимает больше времени. Пропадание истории тоже не очень удобно. Возможно, эти проблемы можно как-то решить через Docker Volumes, но они слишком незначительные для меня, я не заморачивался.

Также нужно понимать, что Docker образ, используемый для разработки, нежелательно использовать для разворачивания приложения в продакшн, так как в дев контейнере будет много лишнего (например линтеры).

## Итог

Контейнеры разработки — эффективный способ поддерживать дев среду у всех разработчиков на проекте в идентичном состоянии. Достаточно одному члену команды запрограммировать настройку дев среды, и все остальные получат необходимые инструменты без лишних усилий, и они будут автоматически обновляться. Больше информации на [containers.dev](https://containers.dev/).
