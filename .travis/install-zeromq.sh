#!/bin/bash
cd $(dirname $0)
wget http://download.zeromq.org/zeromq-$ZMQVER.tar.gz
tar -xf zeromq-$ZMQVER.tar.gz
cd zeromq-$ZMQVER
./configure
make
