#!/bin/bash

cd /vagrant/source
make
cd /vagrant/libatp
chmod 755 vardimn
chmod 755 vardim
dos2unix vardimn
./vardimn listsize.ylk
make
cp startup /vagrant/examples/LN450
cd /vagrant/examples/LN450
#gdb --args /vagrant/libatp/tpbig BOTH test_ESSA.atp s -r
time /vagrant/libatp/tpbig BOTH LN450_2.atp s -r
