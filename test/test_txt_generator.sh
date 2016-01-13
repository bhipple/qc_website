#!/bin/sh
# This small script generates some fake data to test the website.

FORMAT="--pretty=%an|%H|%cr|%s"

# Use the last couple commits from my dotfiles repo and this repo
git log -n 7 "$FORMAT" > qc_website.txt

cd ~/dotfiles
dotfiles=$(git log -n 3 "$FORMAT")
cd -
echo "$dotfiles" > dotfiles.txt

# Regenerate the tar for the website to extract
rm -f ./*.tar.gz
tar -cf ../src/archive-test.tar.gz ./*.txt
rm -f ./*.txt ../src/*.txt
