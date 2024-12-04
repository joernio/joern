FROM alpine:latest

# dependencies
RUN apk update && apk upgrade && apk add --no-cache openjdk17-jdk python3 git curl gnupg bash nss ncurses php
RUN ln -sf python3 /usr/bin/python

# sbt
ENV SBT_VERSION 1.10.3
ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${SBT_HOME}/bin
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz" | gunzip | tar -x -C /usr/local

# building joern
RUN git clone https://github.com/joernio/joern && cd joern && sbt stage
WORKDIR /joern
