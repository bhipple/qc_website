#!/bin/sh
cp img/*.png ~/dotfiles/quicklisp/dists/quicklisp/software/hunchentoot-1.2.34/www/img/
cp src/js/* ~/dotfiles/quicklisp/dists/quicklisp/software/hunchentoot-1.2.34/www/

if ! [ -f configs/config.lisp ]; then
    echo "No config found. Linking test config."
    ln -s "$(pwd)"/configs/test.lisp configs/config.lisp
fi
