#!/usr/bin/env sh
set -eu

SCRIPT_ABS_PATH=$(readlink -f "$0")
SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")

# Check required tools are installed.
check_installed() {
  if ! type "$1" > /dev/null; then
    echo "Please ensure you have $1 installed."
    exit 1
  fi
}
check_installed "sbt"
check_installed "curl"


# Confirm install with user.
JOERN_DEFAULT_INSTALL_DIR=~/bin/joern
JOERN_DEFAULT_LINK_DIR="/usr/local/bin"

echo -n "This script will build Joern and download the FuzzyC2CPG preprocessor. Proceed? [Y/n]: "
read -r JOERN_PROMPT_ANSWER

if [ "$JOERN_PROMPT_ANSWER" != "Y" ] && [ "$JOERN_PROMPT_ANSWER" != "y" ]; then
  exit 0
fi

echo -n "Enter an install location or press enter for the default ($JOERN_DEFAULT_INSTALL_DIR): "
read -r JOERN_INSTALL_DIR

if [ "$JOERN_INSTALL_DIR" = "" ]; then
  JOERN_INSTALL_DIR=$JOERN_DEFAULT_INSTALL_DIR
fi

mkdir -p $JOERN_INSTALL_DIR

echo -n "Would you like to create a symlink to the Joern tools? (default n) [Y/n]: "
read -r JOERN_LINK_ANSWER

if [ "$JOERN_LINK_ANSWER" = "Y" ] || [ "$JOERN_LINK_ANSWER" = "y" ]; then
  echo -n "Where would you like to link the Joern tools? (default $JOERN_DEFAULT_LINK_DIR): "
  read -r JOERN_LINK_DIR
  if [ "$JOERN_LINK_DIR" = "" ]; then
    JOERN_LINK_DIR=$JOERN_DEFAULT_LINK_DIR
  fi
fi

# Build and extract the Joern CLI & Server
cd "$SCRIPT_ABS_DIR"; sbt clean createDistribution; cd - > /dev/null;
unzip -qo -d "$JOERN_INSTALL_DIR" "$SCRIPT_ABS_DIR"/joern-cli.zip
unzip -qo -d "$JOERN_INSTALL_DIR" "$SCRIPT_ABS_DIR"/joern-server.zip

# Link to JOERN_LINK_DIR if desired by the user
if [ -n "${JOERN_LINK_DIR+dummy}" ]; then
  echo "Creating symlinks to Joern tools in $JOERN_LINK_DIR"
  ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern "$JOERN_LINK_DIR"
  ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-cpg2scpg "$JOERN_LINK_DIR"
  ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-parse "$JOERN_LINK_DIR"
  ln -sf "$JOERN_INSTALL_DIR"/joern-server/joernd "$JOERN_LINK_DIR"
fi

echo "Install complete!"
