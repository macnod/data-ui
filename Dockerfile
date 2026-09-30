# macnod/data-ui

#
# Stage 1: frontend build
#
FROM node:22-slim AS web-build

WORKDIR /web
COPY web/package.json web/package-lock.json ./
RUN npm ci
COPY web/ ./
RUN npm run build
# Build output is now in /web/dist

#
# Stage 2: runtime
#
FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive

ARG ROSWELL_VERSION="v23.10.14.114"
ARG SBCL_VERSION="2.5.10"
ARG ROSWELL_URL_PREFIX="https://github.com/roswell/roswell/releases/download"

RUN apt update && apt upgrade -y && apt install -y \
    ack \
    automake \
    build-essential \
    bzip2 \
    curl \
    emacs-nox \
    git \
    gnupg \
    jq \
    libcurl4-openssl-dev \
    tar \
    tree \
    vim \
    zlib1g-dev

# Install Roswell
RUN url="${ROSWELL_URL_PREFIX}/${ROSWELL_VERSION}/roswell_${ROSWELL_VERSION#v}-1_amd64.deb" \
    && curl -fsSL "${url}" -o roswell.deb \
       || { echo "Failed to download Roswell ${ROSWELL_VERSION}"; exit 1; } \
    && dpkg -i roswell.deb \
    && rm roswell.deb

# Install SBCL
# ulimit -n: BuildKit RUN processes inherit the docker daemon's
# LimitNOFILE; when that is unlimited (systemd default on Ubuntu 26.04),
# SBCL's startup fd sweep (close() across the whole fd space) takes
# minutes per invocation. Clamp it to 1M — harmless, and builds run
# ~100x faster on such hosts.
RUN ulimit -n 1048576 && ros install "sbcl-bin/${SBCL_VERSION}" && ros use "sbcl-bin/${SBCL_VERSION}"

# 3rd-party packages (any order ok, so alphabetical)
RUN ulimit -n 1048576 && ros install babel
RUN ulimit -n 1048576 && ros install cl-base64
RUN ulimit -n 1048576 && ros install cl-csv
RUN ulimit -n 1048576 && ros install cl-ppcre
RUN ulimit -n 1048576 && ros install cl-unicode
RUN ulimit -n 1048576 && ros install drakma
RUN ulimit -n 1048576 && ros install fiveam
RUN ulimit -n 1048576 && ros install hunchentoot
RUN ulimit -n 1048576 && ros install ironclad
RUN ulimit -n 1048576 && ros install jose
RUN ulimit -n 1048576 && ros install mgl-pax
RUN ulimit -n 1048576 && ros install postmodern
RUN ulimit -n 1048576 && ros install swank
RUN ulimit -n 1048576 && ros install trivial-utf-8
RUN ulimit -n 1048576 && ros install uiop
RUN ulimit -n 1048576 && ros install yason

# macnod packages (specific order important here)
RUN ulimit -n 1048576 && ros install macnod/dc-dlist
RUN ulimit -n 1048576 && ros install macnod/dc-ds
RUN ulimit -n 1048576 && ros install macnod/dc-time
RUN ulimit -n 1048576 && ros install macnod/p-log
RUN ulimit -n 1048576 && ros install macnod/dc-eclectic
RUN ulimit -n 1048576 && ros install macnod/rbac

# data-ui package
COPY . /root/.roswell/local-projects/data-ui/
RUN ulimit -n 1048576 && ros run -- --eval "(ql:register-local-projects)" --quit
# Pre-compile at build time so container start is fast. Without this,
# every container start recompiles the system, which is slow enough to
# trip the liveness probe during first-boot database initialization.
#
# Failure must exit nonzero, or docker build ships broken images: plain
# `ros run --eval ... --quit` exits 0 even when the require aborts.
# Likewise for data-ui.lisp's *doc-root* error: build containers have no
# DOCUMENT_ROOT, so mkdir the defaults first and let load-time checks run.
RUN ulimit -n 1048576 && mkdir -p /app/shared-files /app/temp-files \
    && ros run -- --disable-debugger \
       --eval '(setf asdf:*compile-file-failure-behaviour* :error)' \
       --eval '(handler-case (progn (require :data-ui) (uiop:quit 0)) (error (c) (format t "data-ui load failed: ~a" c) (uiop:quit 1)))' \
       --quit

# Frontend (served by the Lisp server; see WEB_DIRECTORY, default /app/web)
COPY --from=web-build /web/dist /app/web

# --disable-debugger: on unhandled error, print a backtrace and exit
# instead of hanging at an interactive debugger prompt until the
# liveness probe kills the container.
ENTRYPOINT [ \
    "ros", "run", "--", \
    "--disable-debugger", \
    "--eval", "(require :data-ui)", \
    "--eval", "(data-ui::main)" \
]
