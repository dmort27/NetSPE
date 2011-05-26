export WEBROOT=/Library/WebServer/Documents/
export CGIROOT=/Library/WebServer/CGI-Executables/

sudo mkdir -p $CGIROOT"netspe"
sudo mkdir -p $DOCROOT"netspe"
sudo cp dist/build/derivation.cgi/derivation.cgi $CGIROOT"netspe/"
sudo cp -r netspe/* $WEBROOT"netspe/"
sudo chmod -Rv 755 $WEBROOT"netspe/"