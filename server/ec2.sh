#!/bin/bash
export DEBIAN_FRONTEND=noninteractive
echo "Updating packages."
apt-get update
apt-get -y install build-essential libboost-all-dev libeigen3-dev \
  libjsoncpp-dev make
echo "Cloning latest code from GitHub."
git clone --recursive https://github.com/j3camero/galaxyatlas
cd galaxyatlas/server/
sed -i 's/8080/80/' main.cc
echo "Compiling the webserver."
make
echo "Running the webserver."
./galaxyatlas &
echo "Done setting up webserver."
