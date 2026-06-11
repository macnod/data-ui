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
RUN ros install "sbcl-bin/${SBCL_VERSION}" && ros use "sbcl-bin/${SBCL_VERSION}"

# 3rd-party packages (any order ok, so alphabetical)
RUN ros install babel
RUN ros install cl-base64
RUN ros install cl-csv
RUN ros install cl-ppcre
RUN ros install cl-unicode
RUN ros install drakma
RUN ros install fiveam
RUN ros install hunchentoot
RUN ros install ironclad
RUN ros install jose
RUN ros install mgl-pax
RUN ros install postmodern
RUN ros install swank
RUN ros install trivial-utf-8
RUN ros install uiop
RUN ros install yason

# macnod packages (specific order important here)
RUN ros install macnod/dc-dlist
RUN ros install macnod/dc-ds
RUN ros install macnod/dc-time
RUN ros install macnod/p-log
RUN ros install macnod/rbac
RUN ros install macnod/dc-eclectic

# data-ui package
COPY . /root/.roswell/local-projects/data-ui/
RUN ros run -- --eval "(ql:register-local-projects)" --quit

# Frontend (served by the Lisp server; see WEB_DIRECTORY, default /app/web)
COPY --from=web-build /web/dist /app/web

ENTRYPOINT [ \
    "ros", "run", "--", \
    "--eval", "(require :data-ui)", \
    "--eval", "(data-ui::main \"default-model\")" \
]
