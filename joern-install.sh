#!/usr/bin/env sh
set -eu

if [ -z ${SUDO_UID+x} ]; then
  USER_ID=$(id -u)
  else
  USER_ID=$SUDO_UID
fi

if [ "$(uname)" = 'Darwin' ]; then
  # get script location
  # https://unix.stackexchange.com/a/96238
  if [ "${BASH_SOURCE:-x}" != 'x' ]; then
    this_script=$BASH_SOURCE
  elif [ "${ZSH_VERSION:-x}" != 'x' ]; then
    setopt function_argzero
    this_script=$0

  elif eval '[[ -n ${.sh.file} ]]' 2>/dev/null; then
    eval 'this_script=${.sh.file}'
  else
    echo 1>&2 "Unsupported shell. Please use bash, ksh93 or zsh."
    exit 2
  fi
  relative_directory=$(dirname "$this_script")
  SCRIPT_ABS_DIR=$(cd "$relative_directory" && pwd)
else
  SCRIPT_ABS_PATH=$(readlink -f "$0")
  SCRIPT_ABS_DIR=$(dirname "$SCRIPT_ABS_PATH")
fi

# Check required tools are installed.
check_installed() {
  if ! type "$1" > /dev/null; then
    echo "Please ensure you have $1 installed."
    exit 1
  fi
}

INTERACTIVE=false
NO_DOWNLOAD=false
REINSTALL=false
INSTALL_DEFAULT_PLUGINS=true

while test $# -gt 0
do
    case "$1" in
        --interactive)
          INTERACTIVE=true
            ;;
        --no-download)
          NO_DOWNLOAD=true
            ;;
        --reinstall)
          REINSTALL=true
            ;;
        --without-plugins)
            INSTALL_DEFAULT_PLUGINS=false
            ;;
        --install-dir=*)
            CUSTOM_INSTALL_DIR="${1#*=}"
          ;;
        --version=*)
          CUSTOM_VERSION="${1#*=}"
          ;;
        --*) echo "bad option $1"
            ;;
        *) echo "argument $1"
            ;;
    esac
    shift
done

if [ "$(whoami)" != "root" ]; then
  JOERN_DEFAULT_INSTALL_DIR=~/bin/joern
else
  JOERN_DEFAULT_INSTALL_DIR=/opt/joern
fi

JOERN_DEFAULT_LINK_DIR="/usr/local/bin"
JOERN_DEFAULT_VERSION=""

if [ $INTERACTIVE = false ]; then
  echo "non-interactive mode, using defaults"
  JOERN_INSTALL_DIR="${CUSTOM_INSTALL_DIR:-$JOERN_DEFAULT_INSTALL_DIR}"
  JOERN_VERSION="${CUSTOM_VERSION:-$JOERN_DEFAULT_VERSION}"
  JOERN_LINK_DIR=$JOERN_DEFAULT_LINK_DIR
  echo "Installation dir: $JOERN_INSTALL_DIR"
  echo "Version: $JOERN_VERSION"
  echo "Symbolic links in: $JOERN_LINK_DIR"
else
    # Confirm install with user.
    printf "This script will download and install the Joern tools on your machine. Proceed? [Y/n]: "
    read -r JOERN_PROMPT_ANSWER

    if [ "$JOERN_PROMPT_ANSWER" = "N" ] || [ "$JOERN_PROMPT_ANSWER" = "n" ]; then
	exit 0
    fi

    printf '%s' "Enter an install location or press enter for the default ($JOERN_DEFAULT_INSTALL_DIR): "
    read -r JOERN_INSTALL_DIR

    if [ "$JOERN_INSTALL_DIR" = "" ]; then
	    JOERN_INSTALL_DIR=$JOERN_DEFAULT_INSTALL_DIR
    fi

    printf "Would you like to create a symlink to the Joern tools? [y/N]: "
    read -r JOERN_LINK_ANSWER

    if [ "$JOERN_LINK_ANSWER" = "Y" ] || [ "$JOERN_LINK_ANSWER" = "y" ]; then
    	printf '%s' "Where would you like to link the Joern tools? (default $JOERN_DEFAULT_LINK_DIR): "
	    read -r JOERN_LINK_DIR
	    if [ "$JOERN_LINK_DIR" = "" ]; then
	      JOERN_LINK_DIR=$JOERN_DEFAULT_LINK_DIR
  	  fi
    fi

    printf "Please enter a Joern version/tag or press enter for the latest version: "
    read -r JOERN_VERSION
fi

# Check if installation is already present and be careful about removing it
if [ -d "$JOERN_INSTALL_DIR/joern-cli" ]; then
  echo "Installation already exists at: $JOERN_INSTALL_DIR/joern-cli"
  if [ $INTERACTIVE = true ]; then
    printf "Should I remove it? [y/N]"
    read -r ANSWER
    if [ $ANSWER = "y" ] || [ $ANSWER = "Y" ]; then
      echo "rm -rf $JOERN_INSTALL_DIR/joern-cli"
      rm -rf "$JOERN_INSTALL_DIR/joern-cli"
    else
        exit
    fi
  else
    if [ $REINSTALL = false ]; then
      echo "Please remove it first or run with --reinstall"
      exit
    else
      echo "Removing: $JOERN_INSTALL_DIR/joern-cli"
      echo "rm -rf $JOERN_INSTALL_DIR/joern-cli"
      rm -rf "$JOERN_INSTALL_DIR/joern-cli"
    fi
  fi
fi

if [ ! -d $JOERN_INSTALL_DIR ]; then
  echo "$JOERN_INSTALL_DIR does not exist. Creating"
  mkdir -p $JOERN_INSTALL_DIR
fi

mkdir -p $JOERN_INSTALL_DIR
# Download and extract the Joern CLI

check_installed "curl"
check_installed "unzip"

if [ $NO_DOWNLOAD = true ]; then
    sbt createDistribution
    sbt clean
else
  if [ "$JOERN_VERSION" = "" ]; then
    curl -L "https://github.com/joernio/joern/releases/latest/download/joern-cli.zip" -o "$SCRIPT_ABS_DIR/joern-cli.zip"
  else
    curl -L "https://github.com/joernio/joern/releases/download/$JOERN_VERSION/joern-cli.zip" -o "$SCRIPT_ABS_DIR/joern-cli.zip"
  fi
fi

unzip -qo -d "$JOERN_INSTALL_DIR" "$SCRIPT_ABS_DIR"/joern-cli.zip

if [ $INTERACTIVE = false ] && [ "$(whoami)" != "root" ]; then
  echo "==============================================================="
  echo "you are not root and you are running non-interactively - will not create system symlinks"
  echo "==============================================================="
else
    # Link to JOERN_LINK_DIR if desired by the user
    if [ -n "${JOERN_LINK_DIR+dummy}" ]; then
	    echo "Creating symlinks to Joern tools in $JOERN_LINK_DIR"
	    echo "If you are not root, please enter your password now:"
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-parse "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/c2cpg.sh "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/ghidra2cpg "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/jssrc2cpg.sh "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/javasrc2cpg "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/jimple2cpg "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/kotlin2cpg "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/php2cpg "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/rubysrc2cpg "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/pysrc2cpg "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-export "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-flow "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-scan "$JOERN_LINK_DIR" || true
	    sudo ln -sf "$JOERN_INSTALL_DIR"/joern-cli/joern-slice "$JOERN_LINK_DIR" || true
    fi
fi

chown -R $USER_ID $JOERN_INSTALL_DIR

if [ $INSTALL_DEFAULT_PLUGINS = true ]; then
  echo "Installing default plugins"
  CURDIR=$(pwd)
  cd $JOERN_INSTALL_DIR/joern-cli
  ./joern-scan --updatedb --dbversion $JOERN_VERSION
  cd "$CURDIR"
fi

echo "Install complete!"
