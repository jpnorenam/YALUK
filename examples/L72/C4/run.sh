#!/bin/bash

cd /vagrant/source
make
cd /vagrant/libatp
chmod 755 vardimn
chmod 755 vardim
dos2unix vardimn
./vardimn listsize.ylk
make
cp startup /vagrant/examples/L72/C4/
cd /vagrant/examples/L72/C4/
#gdb --args /vagrant/libatp/tpbig BOTH test_ESSA.atp s -r
time /vagrant/libatp/tpbig BOTH L72_502_circuitC4.0.atp s -r
