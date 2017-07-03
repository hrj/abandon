JAVA_CMD=java

if [ $JAVA_HOME ]
then
  JAVA_CMD=$JAVA_HOME/bin/java;
fi

BASEDIR=`dirname $0`

EXTRA_OPTIONS=""
if [ "$(uname -s)" == "Darwin" ]
then
  # The dock name won't actually work until https://bugs.openjdk.java.net/browse/JDK-8029440 is fixed
  EXTRA_OPTIONS='-Xdock:icon=abandon.png -Xdock:name=Abandon -Dcom.apple.mrj.application.apple.menu.about.name=Abandon -Dapple.awt.application.name=Abandon'
fi

$JAVA_CMD ${EXTRA_OPTIONS} -jar ${BASEDIR}/lib/abandon.jar -g $*
