@echo off

set "SCRIPT_ABS_DIR=%~dp0"
set "SCRIPT=%SCRIPT_ABS_DIR%frontends\jimple2cpg\bin\jimple2cpg.bat"

"%SCRIPT%" "-J-XX:+UseG1GC" "-J-XX:CompressedClassSpaceSize=128m" "-Dlog4j.configurationFile=%SCRIPT_ABS_DIR%conf\log4j2.xml" %*
