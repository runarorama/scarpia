@echo off
set SCRIPT_DIR=%~dp0
java %SBT_OPTS% -Xmx1024m -Dhttp.proxyHost=corp-proxy.mhc -Dhttp.proxyPort=8080 -Dhttp.proxyUser=%PROXY_USER% -Dhttp.proxyPassword=%PROXY_PASSWORD% -Dfile.encoding=UTF-8 -Xss4M -Xmx1024M -jar "%SCRIPT_DIR%sbt-launcher.jar" %*
