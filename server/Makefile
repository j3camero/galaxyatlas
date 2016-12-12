# Makefile for galaxyatlas

EXEC_NAME = galaxyatlas

#MAGICKCONF = /usr/lib/x86_64-linux-gnu/ImageMagick-6.8.9/bin-Q16/MagickCore-config

CXX = g++
CXX_FLAGS = --std=c++11 -Wall
LINUX_INC = -I/usr/include/eigen3 -I/usr/include/ImageMagick-6 -I/usr/include/x86_64-linux-gnu/ImageMagick-6 -I/usr/include/jsoncpp
MAC_INC = -I/usr/local/Cellar/eigen/3.2.8/include/eigen3 -I/usr/local/Cellar/imagemagick/6.9.5-0/include/ImageMagick-6
INC = $(LINUX_INC) $(MAC_INC) -I./Simple-Web-Server

LDFLAGS = -pthread -lm
BOOSTLD = -lboost_system -lboost_thread -lboost_filesystem -lboost_date_time -ljsoncpp
MAGICKLD = -lMagick++-6.Q16 -lMagickCore-6.Q16


DEFINES = -DMAGICKCORE_QUANTUM_DEPTH=16 -DMAGICKCORE_HDRI_ENABLE=0

COMMON_SRC = startree.cc
MAIN_SRC = main.cc $(COMMON_SRC) server_util.cpp
BRUTE_SRC = brute_force.cc $(COMMON_SRC)

all: main

main:
	$(CXX) $(CXX_FLAGS) -o $(EXEC_NAME) $(MAIN_SRC) $(INC) $(LDFLAGS) $(BOOSTLD)

bruteforce:
	$(CXX) $(CXX_FLAGS) -o bruteforce $(BRUTE_SRC) $(DEFINES) $(INC) $(LDFLAGS) $(MAGICKLD)

clean:
	rm -f galaxyatlas bruteforce
