
# Test
# docker build -f $(pwd)/.devcontainer/Dockerfile -t vsc-test $(pwd)/.devcontainer
# docker run --rm -it -v $(pwd):/workspace/ vsc-test /bin/bash


FROM tencentos/tencentos_server31:latest

# install git git-lfs
RUN yum -y install zlib zlib-devel curl perl-ExtUtils-CBuilder perl-ExtUtils-MakeMaker gcc curl-devel expect make wget gettext zip unzip \
    && yum -y remove git \
    && yum -y install git git-lfs \
    && git lfs install

# install jdk11 sbt
RUN mkdir -p /data/App \
    && cd /data/App \
    && wget https://github.com/sbt/sbt/releases/download/v1.6.1/sbt-1.6.1.zip \
    && unzip *.zip \
    && rm *.zip \
    && mv sbt/ sbt-1.6.1/ \
    && wget https://github.com/Tencent/TencentKona-11/releases/download/kona11.0.15/TencentKona-11.0.15.b1-jdk_linux-x86_64.tar.gz \
    && tar zxvf *.tar.gz \
    && rm *.tar.gz

# ENV
ENV LANG=en_US.UTF-8 \
    JAVA_HOME=/data/App/TencentKona-11.0.15.b1 \
    PATH=/data/App/sbt-1.6.1/bin:/data/App/TencentKona-11.0.15.b1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
