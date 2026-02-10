## Mu dependecies

gmime-2.6
xapian

## Mu compilation
autoreconf -i
./configure --prefix=$HOME/.emacs.d/mu
make check
make install

