#!/bin/sh
# This small script generates some fake data

git log -n 5 --pretty='%an|%H|%ci|%s' > qc_website.txt

cd ~/dotfiles
dotfiles=$(git log -n 5 --pretty='%an|%H|%ci|%s')
cd -
echo "$dotfiles" > dotfiles.txt
