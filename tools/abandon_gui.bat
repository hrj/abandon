@ECHO OFF
SET BASEDIR=%~dp0

java -jar "%BASEDIR%\start.jar" -g %*
