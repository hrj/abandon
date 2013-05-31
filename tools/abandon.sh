BASEDIR=`dirname $0`

JFXRT_FILE="$JAVA_HOME/jre/lib/jfxrt.jar"

if [ -f $JFXRT_FILE ]
then
  java -cp $JFXRT_FILE:${BASEDIR}/abandon.jar co.uproot.abandon.Main $*
else
  if [ -z "$JAVA_HOME" ]; then
    echo "JavaFX Runtime was not found and JAVA_HOME was not set."
  fi  
  echo "Ensure you have Oracle Java 7 or higher, or OpenJDK 8 or higher installed."
fi
