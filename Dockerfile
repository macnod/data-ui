# macnod/data-ui

FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive

ROSWELL_VERSION="23.10.14.114"
SBCL_VERSION="2.5.10"

ROSWELL_URL_PREFIX="https://github.com/roswell/roswell/releases/download"

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
RUN curl -fsSL "${url}" -o roswell.deb || { echo "Failed to download Roswell $roswell_version"; exit 1; }
RUN sudo dpkg -i roswell.deb
RUN rm roswell.deb

# 3rd-party packages
RUN ros install postmodern
RUN ros install uiop
RUN ros install cl-ppcre
RUN ros install hunchentoot
RUN ros install swank
RUN ros install spinneret
RUN ros install trivial-utf-8
RUN ros install ironclad
RUN ros install babel
RUN ros install jose
RUN ros install cl-csv
RUN ros install cl-unicode

# macnod packages
RUN ros install macnod/dc-ds
RUN ros install macnod/dc-dlist
RUN ros install macnod/dc-eclectic
RUN ros install macnod/rbac
