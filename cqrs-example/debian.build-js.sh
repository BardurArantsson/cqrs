#!/bin/sh -e
sudo apt-get install npm nodejs-legacy
cd static/
npm install browserify minifyify babelify react classnames reflux
npm run-script bundle
