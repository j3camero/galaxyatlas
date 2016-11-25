# Makefile for galaxyatlas

EXEC_NAME = galaxyatlas

CXX = g++
CXX_FLAGS = --std=c++11 -Wall
INC = -I/usr/include/eigen3 -I/usr/include/ImageMagick-6 -I/usr/include/x86_64-linux-gnu/ImageMagick-6
LDFLAGS = -pthread -lm -lMagick++-6.Q16

DEFINES = -DMAGICKCORE_QUANTUM_DEPTH=16 -DMAGICKCORE_HDRI_ENABLE=0

SRC = main.cc startree.cc

all: main

main:
	$(CXX) $(CXX_FLAGS) -o $(EXEC_NAME) $(SRC) $(INC) $(LDFLAGS) $(DEFINES)

clean:
	rm -f galaxyatlas
