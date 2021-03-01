SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

if [ `uname -m` == 'x86_64' ]; then
  ARCH='64'
else
  ARCH='32'
fi

CMXS=$DIR/../cmxs
HX="hx-$( haxe --version 2>&1 )"

# Remove previous build
if [ -d "$CMXS/$HX/Windows$ARCH" ]; then
  rm -r $CMXS/$HX/Windows$ARCH
fi
# Create destination directory
if [ ! -d "$CMXS/$HX" ]; then
  mkdir $CMXS/$HX
fi

mv -T $CMXS/Windows $CMXS/$HX/Windows$ARCH