# Builds and runs the Glowing Cube Demo.

# Stop if any command fails, instead of blindly continuing.
set -e

# Get npm dependencies.
npm update
npm install

# Package up the client-side static files.
rm -rf static
mkdir -p static
cp index.html static/
cp style.css static/
browserify client.js -o static/client.min.js

# Run the webserver.
node server.js
