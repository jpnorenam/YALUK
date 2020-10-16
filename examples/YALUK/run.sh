#!/bin/bash

cd /vagrant/source
make
cd /vagrant/libatp
chmod 755 vardimn
./vardimn listsize.big
make
cp startup /vagrant/examples/YALUK
cd /vagrant/examples/YALUK
/vagrant/libatp/tpbig BOTH test.atp s -r