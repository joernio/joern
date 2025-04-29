FROM almalinux:latest

RUN yum install wget unzip git -y

# install jdk21 sbt
RUN mkdir -p /data/App \
    && cd /data/App \
    && wget https://github.com/sbt/sbt/releases/download/v1.10.11/sbt-1.10.11.zip \
    && unzip *.zip \
    && rm *.zip \
    && mv sbt/ sbt-1.10.11/ \
    && wget https://download.java.net/java/21/archive/jdk-21.0.7_linux-x64_bin.tar.gz \
    && tar zxvf *.tar.gz \
    && rm *.tar.gz

ENV LANG=en_US.UTF-8 \
    JAVA_HOME=/data/App/jdk-21.0.7 \
    PATH=/data/App/sbt-1.10.11/bin:/data/App/jdk-21.0.7/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
