# YALUK
Yaluk is an [ATP] external model to compute induced voltages
in transmission/distribution due to lightning activity.

## How to use
### Dependencies
```
sudo apt install \
gcc g++ gfortran pkg-config make \
libgl1 libxm4 libncurses5 dos2unix \
```
### Build
`cd source & make`

```
cd ../libatp
chmod 755 vardim vardimn
dos2unix vardimn
./vardimn listsize.ylk
make
```

### Python launcher
```
sudo apt install python3 pip3
pip install requirements.txt
```

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
