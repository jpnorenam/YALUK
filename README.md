# YALUK
Yaluk is an [ATP] external model to compute induced voltages
in transmission/distribution due to lightning activity.

## How to use
### Dependencies
```
sudo apt install \
gcc g++ gfortran pkg-config make \
libgl1 libxm4 libncurses5 dos2unix
```
### Build
First let's build libyaluk.a
```
cd source
make
```
Now let's build and link libapt
```
cd ../libatp
chmod 755 vardim vardimn
dos2unix vardimn
./vardimn listsize.ylk
make
```

### Python launcher
In the root directory of the repository
```
sudo apt install python3
export PWD=$(pwd)
python yaluk_run.py -nLN450 -w$PWD/examples/LN450 -j4 -by -a$PWD/libatp -y$PWD/source
```
This is an example, do `python yaluk_run.py -h` for some hints. \
Have a coffee :coffee:. In `$PWD/examples/LN450/results` you will find your results safe and sound.

### Using Vagrant
There is a [VagrantFile] in the repository you can use
```
vagrant up
vagrant ssh
cd /vagrant
```
remember to configure the following parameters according to your host machine.
```
config.vm.provider "virtualbox" do |vb|
    vb.name = "YALUK"
    vb.gui = false
    vb.cpus = 5
    vb.memory = "6144"
  end
```
and follow the python launcher steps.

## Copyright
2020, Laboratorio de Gestión de Sistemas en Tiempo Real, Facultad de Minas, Universidad Nacional de Colombia

## Contacto

[![LGSTR Logo](docs/LGSTR_logo.png)](https://sites.google.com/unal.edu.co/lab-gstr/)

- Ernesto Pérez <eperezg@unal.edu.co>
- Edison Soto <easotor@unal.edu.co>
- Juan Pablo Noreña <jpnorenam@unal.edu.co>

[Laboratorio de Gestión de Sistemas en Tiempo Real](https://sites.google.com/unal.edu.co/lab-gstr/) \
[Facultad de Minas](https://minas.medellin.unal.edu.co/) \
[Universidad Nacional de Colombia](https://unal.edu.co/)

[ATP]: https://www.emtp.org/index.php
[VagrantFile]: https://www.vagrantup.com/docs/installation
