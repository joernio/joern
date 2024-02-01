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
# https://s01.oss.sonatype.org/content/groups/public/io/joern/eclipse-cdt-core/
# https://repo1.maven.org/maven2/io/joern/eclise-cdt-core/
# https://github.com/digimead/sbt-osgi-manager/blob/master/src/main/scala/sbt/osgi/manager/tycho/ResolveP2.scala

# adapt for every release
JAR_URL='https://ci.eclipse.org/cdt/job/cdt/job/main/353/artifact/releng/org.eclipse.cdt.repo/target/repository/plugins/org.eclipse.cdt.core_8.4.0.202401242025.jar'
CUSTOM_RELEASE_VERSION='8.4.0.202401242025'

# adapt when releasing from a different machine: the server id from your local ~/.m2/settings.xml
REPO_ID=sonatype-nexus-staging-joern




LOCAL_JAR="org.eclipse.cdt.core-$CUSTOM_RELEASE_VERSION.jar"
echo "downloading jar from $JAR_URL to $LOCAL_JAR"
wget $JAR_URL -O $LOCAL_JAR

# install into local maven repo, mostly to generate a pom
mvn install:install-file -DgroupId=io.joern -DartifactId=eclipse-cdt-core -Dpackaging=jar -Dversion=$CUSTOM_RELEASE_VERSION -Dfile=$LOCAL_JAR -DgeneratePom=true
cp ~/.m2/repository/io/joern/eclipse-cdt-core/$CUSTOM_RELEASE_VERSION/eclipse-cdt-core-$CUSTOM_RELEASE_VERSION.pom pom.xml

# add pom-extra to pom.xml, to make sonatype happy
head -n -1 pom.xml > pom.tmp
cat pom.tmp pom-extra > pom.xml
rm pom.tmp

# create empty jar for "sources" - just to make sonatype happy
zip empty.jar LICENSE

# sign and upload artifacts to sonatype staging
SONATYPE_URL=https://s01.oss.sonatype.org/service/local/staging/deploy/maven2/
mvn gpg:sign-and-deploy-file -Durl=$SONATYPE_URL -DrepositoryId=$REPO_ID -DpomFile=pom.xml -Dclassifier=sources -Dfile=empty.jar
mvn gpg:sign-and-deploy-file -Durl=$SONATYPE_URL -DrepositoryId=$REPO_ID -DpomFile=pom.xml -Dclassifier=javadoc -Dfile=empty.jar
mvn gpg:sign-and-deploy-file -Durl=$SONATYPE_URL -DrepositoryId=$REPO_ID -DpomFile=pom.xml -Dfile=$LOCAL_JAR

# remove temporary working artifacts
rm $LOCAL_JAR pom.xml empty.jar *.asc

echo "artifacts are now published to sonatype staging. next step: log into https://s01.oss.sonatype.org -> staging repositories -> select the right one -> close -> release"
echo "you can monitor the maven sync status on https://s01.oss.sonatype.org/content/groups/public/io/joern/eclipse-cdt-core/ and https://repo1.maven.org/maven2/io/joern/eclipse-cdt-core/"
echo "once it's synchronised to maven central (repo1), update the cdt-core version in 'joern/joern-cli/frontends/c2cpg/build.sbt'"

