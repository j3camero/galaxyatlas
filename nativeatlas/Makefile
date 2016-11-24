# Makefile for galaxyatlas

EXEC_NAME = galaxyatlas

CXX_FLAGS = --std=c++11 -Wall
INC = -I/usr/include/eigen3
LDFLAGS = -pthread -lm

SRC = main.cc startree.cc

all: main

main:
	g++ $(CXX_FLAGS) -o $(EXEC_NAME) $(SRC) $(INC) $(LDFLAGS)

clean:
	rm -f galaxyatlas
