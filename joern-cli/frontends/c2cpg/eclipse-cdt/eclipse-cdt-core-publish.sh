#!/usr/bin/env bash
set -e #stop on error
set -o pipefail

# this script downloads a cdt-core release from a configurable location
# (e.g. an eclipse release mirror, or their jenkins CI) and publishes it to
# sonatype, so that we can promote it to maven central:
# context: eclipse uses their own repository format called p2 tycho,
# but tooling is limited
# Some related links:
# https://ci.eclipse.org/cdt/job/cdt/job/main
# https://ci.eclipse.org/cdt/job/cdt/job/main/353/artifact/releng/org.eclipse.cdt.repo/target/repository/plugins/org.eclipse.cdt.core_8.4.0.202401242025.jar
# https://ftp.fau.de/eclipse/tools/cdt/releases/11.4/cdt-11.4.0/plugins/
# 
# https://repo1.maven.org/maven2/io/joern/eclise-cdt-core/
# https://github.com/digimead/sbt-osgi-manager/blob/master/src/main/scala/sbt/osgi/manager/tycho/ResolveP2.scala

# adapt for every release
JAR_URL='https://ci.eclipse.org/cdt/job/cdt/job/main/452/artifact/releng/org.eclipse.cdt.repo/target/repository/plugins/org.eclipse.cdt.core_8.5.0.202410191453.jar'
CUSTOM_RELEASE_VERSION='8.5.0.202410191453'

LOCAL_JAR="org.eclipse.cdt.core-$CUSTOM_RELEASE_VERSION.jar"
echo "downloading jar from $JAR_URL to $LOCAL_JAR"
wget $JAR_URL -O $LOCAL_JAR

# create custom-made maven build just for deploying to maven central
rm -rf build
mkdir build
sed s/__VERSION__/$CUSTOM_RELEASE_VERSION/ pom.xml.template > build/pom.xml
mkdir -p build/src/main/resources
unzip -d build/src/main/resources $LOCAL_JAR

# add an empty dummy class in order to generate sources and javadoc jars
mkdir -p build/src/main/java
echo '/** just an empty placeholder to trigger javadoc generation */
public interface Empty {}' > build/src/main/java/Empty.java

# deploy to sonatype central
pushd build
mvn javadoc:jar source:jar package gpg:sign deploy
popd

echo "release is now published to sonatype central and should get promoted to maven central automatically. For more context go to https://central.sonatype.com/publishing/deployments"
echo "once it's synchronised to maven central (https://repo1.maven.org/maven2/io/joern/eclipse-cdt-core/), update the cdt-core version in 'joern/joern-cli/frontends/c2cpg/build.sbt' to $CUSTOM_RELEASE_VERSION"
