JAVA_CMD=java

if [ $JAVA_HOME ]
then
  JAVA_CMD=$JAVA_HOME/bin/java;
fi

BASEDIR=`dirname $0`

$JAVA_CMD -jar ${BASEDIR}/start.jar $*
