#!/bin/bash
set -e
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

find_haxe_binary_from() {
    local dir=$1 os=$2
    local relative="haxe"
    [[ "$os" == "windows" ]] && relative+=".exe"
    for ((depth=0; depth < 16; depth++)); do
        local candidate="$dir/$relative"
        [[ -x "$candidate" ]] && echo "$candidate" && return
        relative="../$relative"
    done
}

# Get Architecture
ARCH=$(uname -m)

# Get OS
case "$(uname -s)" in
  Linux*) OS='linux' ;;
  Darwin*) OS='mac' ;;
  *) OS='windows' ;;
esac

# Get plugin destinatioin
CMXS=$DIR/../cmxs

# Get Haxe binary file
HAXE_BIN=$(find_haxe_binary_from "$DIR" "$OS")

if [[ -z "$HAXE_BIN" ]]; then
  echo "ERROR: Could not find haxe binary" >&2
  exit 1
fi

# Get Haxe version
HAXE_VER=$("$HAXE_BIN" --version 2>&1)
HAXE_VER=${HAXE_VER%+*} # remove commit information until we can also get it from macros

# Get plugin output directory
case $OS in
  linux) PLUGIN_SOURCE=$CMXS/Linux ;;
  mac) PLUGIN_SOURCE=$CMXS/Mac ;;
  windows) PLUGIN_SOURCE=$CMXS/Windows ;;
esac

HX="hx-$HAXE_VER"
PLUGIN_DESTINATION=$CMXS/$HX/$OS-$ARCH

# Remove previous build
if [ -d "$PLUGIN_DESTINATION" ]; then
  rm -r $PLUGIN_DESTINATION
fi
# Create destination directory
if [ ! -d "$CMXS/$HX" ]; then
  mkdir $CMXS/$HX
fi

mv -T $PLUGIN_SOURCE $PLUGIN_DESTINATION
