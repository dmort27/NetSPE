sudo mkdir -p /Library/WebServer/CGI-Executables/netspe
sudo mkdir /Library/WebServer/Documents/netspe
sudo cp dist/build/derivation.cgi/derivation.cgi /Library/WebServer/CGI-Executables/netspe/
sudo cp -r netspe/* /Library/WebServer/Documents/netspe/