#!/bin/sh
# This small script generates some fake data to test the website.

# Cleanup
rm -rf ../*.tar.gz ../*.txt

FORMAT="--pretty=%an|%H|%ci|%cr|%s"

# Use the last couple commits from my dotfiles repo and this repo
git log -n 7 "$FORMAT" > qc_website.txt

cd ~/dotfiles
dotfiles=$(git log -n 3 "$FORMAT")
cd -
echo "$dotfiles" > dotfiles.txt

# Regenerate the tar for the website to extract
tar -cf ../archive-test.tar.gz ./*.txt
rm -f ./*.txt
