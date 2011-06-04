export WEBROOT=/Library/WebServer/Documents/
export CGIROOT=/Library/WebServer/CGI-Executables/

mkdir -p $CGIROOT"netspe"
mkdir -p $DOCROOT"netspe"
cp dist/build/derivation.cgi/derivation.cgi $CGIROOT"netspe/"
cp dist/build/lint.cgi/lint.cgi $CGIROOT"netspe/"
cp -r netspe/* $WEBROOT"netspe/"
chmod -Rv 755 $WEBROOT"netspe/"