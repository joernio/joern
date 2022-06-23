FROM centos:centos7.9.2009

# install git git-lfs
RUN yum -y install zlib zlib-devel curl perl-ExtUtils-CBuilder perl-ExtUtils-MakeMaker gcc curl-devel expect make wget gettext zip unzip \
    && yum -y remove git \
    && yum -y install git

# install jdk11 sbt
RUN mkdir -p /data/App \
    && cd /data/App \
    && wget https://github.com/sbt/sbt/releases/download/v1.6.1/sbt-1.6.1.zip \
    && unzip *.zip \
    && rm *.zip \
    && mv sbt/ sbt-1.6.1/ \
    && wget https://download.java.net/java/GA/jdk11/13/GPL/openjdk-11.0.1_linux-x64_bin.tar.gz \
    && tar zxvf *.tar.gz \
    && rm *.tar.gz

ENV LANG=en_US.UTF-8 \
    JAVA_HOME=/data/App/jdk-11.0.1 \
    PATH=/data/App/sbt-1.6.1/bin:/data/App/jdk-11.0.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
