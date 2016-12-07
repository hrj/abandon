@ECHO OFF
SET BASEDIR=%~dp0

SET JAVA_CMD=java

if not "%JAVA_HOME%" == "" (
    SET JAVA_CMD="%JAVA_HOME%\bin\java"
)

"%JAVA_CMD%" -jar "%BASEDIR%\lib\abandon.jar" -g %*
