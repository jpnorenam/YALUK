#!/bin/bash
sudo apt update
sudo apt install \
gcc g++ gfortran pkg-config make \
libgl1 libxm4 libncurses5 dos2unix git python3-pip

pip3 install pandas

cd source
make
cd ../libatp
chmod 755 vardimn
chmod 755 vardim
dos2unix vardimn
./vardimn listsize.ylk
make
cd ..

