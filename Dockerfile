FROM alpine:latest

# dependencies
RUN apk update && apk upgrade && apk add --no-cache openjdk8-jre-base python3 git curl gnupg bash nss ncurses
RUN ln -sf python3 /usr/bin/python

# sbt
ENV SBT_VERSION 1.3.13
ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${SBT_HOME}/bin
RUN curl -sL "https://piccolo.link/sbt-$SBT_VERSION.tgz" | gunzip | tar -x -C /usr/local

# building joern
RUN git clone https://github.com/ShiftLeftSecurity/joern.git && cd joern && sbt stage
WORKDIR /joern
