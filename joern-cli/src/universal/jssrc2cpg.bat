@echo off

set "SCRIPT_ABS_DIR=%~dp0"
set "SCRIPT=%SCRIPT_ABS_DIR%frontends\jssrc2cpg\bin\jssrc2cpg.bat"

"%SCRIPT%" -J-XX:+UseG1GC -J-XX:CompressedClassSpaceSize=128m "-Dlog4j.configurationFile=%SCRIPT_ABS_DIR%conf\log4j2.xml" %*
