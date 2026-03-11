FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# Install all dependencies (including nss, ncurses, php)
RUN apt-get update && apt-get install -y openjdk-21-jdk python3 python3-pip git curl gnupg bash libnss3 libncurses6 php-cli build-essential clang libclang-dev libffi-dev pkg-config ca-certificates wget unzip

# Python compatibility
RUN ln -sf /usr/bin/python3 /usr/bin/python

# sbt
ENV SBT_VERSION 1.12.1
ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${SBT_HOME}/bin
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz" | gunzip | tar -x -C /usr/local

# building joern
RUN git clone https://github.com/joernio/joern && cd joern && sbt stage
WORKDIR /joern
