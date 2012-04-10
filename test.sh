#!/bin/bash
make clean
rm mt.pdf
make thesis.pdf
cp thesis.pdf mt.pdf
make clean

evince mt.pdf
#explorer mt.pdf
