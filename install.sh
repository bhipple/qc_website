#!/bin/sh

echo "Copying images and libraries to server directory."
cp img/*.png ~/dotfiles/quicklisp/dists/quicklisp/software/hunchentoot-1.2.34/www/img/
cp js/* ~/dotfiles/quicklisp/dists/quicklisp/software/hunchentoot-1.2.34/www/

echo "Creating directory to store commits."
mkdir -p commits

echo "Allowing SBCL to run on port 80"
sudo setcap 'cap_net_bind_service=+ep' /usr/bin/sbcl

if ! [ -f configs/config.lisp ]; then
    echo "No config found. Linking test config."
    ln -fs "$(pwd)"/configs/test.lisp configs/config.lisp
fi
