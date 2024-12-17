#!/usr/bin/env bash
set -e #stop on error
set -o pipefail

# this script downloads a cdt-core release from a configurable location
# (e.g. an eclipse release mirror, or their jenkins CI) and publishes it to
# sonatype, so that we can promote it to maven central.
# note: we also swap the original CCorePlugin.java for a simplified one
# context: eclipse uses their own repository format called p2 tycho,
# but tooling is limited
# Some related links:
# https://ci.eclipse.org/cdt/job/cdt/job/main
# https://ci.eclipse.org/cdt/job/cdt/job/main/353/artifact/releng/org.eclipse.cdt.repo/target/repository/plugins/org.eclipse.cdt.core_8.4.0.202401242025.jar
# https://ftp.fau.de/eclipse/tools/cdt/releases/11.4/cdt-11.4.0/plugins/
# https://github.com/joernio/joern/pull/5178
# 
# https://repo1.maven.org/maven2/io/joern/eclise-cdt-core/
# https://github.com/digimead/sbt-osgi-manager/blob/master/src/main/scala/sbt/osgi/manager/tycho/ResolveP2.scala

# adapt for every release
JAR_URL='https://ci.eclipse.org/cdt/job/cdt/job/main/452/artifact/releng/org.eclipse.cdt.repo/target/repository/plugins/org.eclipse.cdt.core_8.5.0.202410191453.jar'
CUSTOM_RELEASE_VERSION='8.5.0.202410191453+3'

LOCAL_JAR="org.eclipse.cdt.core-$CUSTOM_RELEASE_VERSION.jar"
echo "downloading jar from $JAR_URL to $LOCAL_JAR"
wget $JAR_URL -O $LOCAL_JAR

# create custom-made maven build just for deploying to maven central
rm -rf build
mkdir build
pushd build
sed s/__VERSION__/$CUSTOM_RELEASE_VERSION/ ../pom.xml.template > pom.xml
mkdir -p src/main/resources
unzip -d src/main/resources ../$LOCAL_JAR \
      -x 'META-INF/*.RSA' 'META-INF/*.SF' \
         'org/eclipse/cdt/core/CCorePlugin*.class'
# passing -x option to exclude some files:
# 1) original signing information, otherwise the jar is unusable because the signature doesn't match
# 2) original CCorePlugin.class because we want to replace it with our simplified version

# add our custom CCorePlugin.java
mkdir -p src/main/java/org/eclipse/cdt/core
cp ../CCorePlugin.java src/main/java/org/eclipse/cdt/core

# deploy to sonatype central
mvn javadoc:jar source:jar package gpg:sign deploy
popd

echo "release is now published to sonatype central and should get promoted to maven central automatically. For more context go to https://central.sonatype.com/publishing/deployments"
echo "once it's synchronised to maven central (https://repo1.maven.org/maven2/io/joern/eclipse-cdt-core/), update the cdt-core version in 'joern/joern-cli/frontends/c2cpg/build.sbt' to $CUSTOM_RELEASE_VERSION"
