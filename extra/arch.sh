SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

# Get Architecture
ARCH=$(uname -m)

# Get OS
case "$OSTYPE" in
  linux*) OS='linux' ;;
  darwin*) OS='mac' ;;
  *) OS='windows' ;;
esac

# Get Haxe version
CMXS=$DIR/../cmxs
cd $DIR/../../..
if [[ "$OS" == "windows" ]]; then
  HX="hx-$( ./haxe.exe --version 2>&1 )"
else
  HX="hx-$( ./haxe --version 2>&1 )"
fi
cd $DIR
HX=${HX%+*} # remove commit information until we can also get it from macros

# Get plugin output directory
case $OS in
  linux) PLUGIN_SOURCE=$CMXS/Linux ;;
  mac) PLUGIN_SOURCE=$CMXS/Mac ;;
  windows) PLUGIN_SOURCE=$CMXS/Windows ;;
esac

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