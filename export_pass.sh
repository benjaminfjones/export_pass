#!/usr/bin/env bash
#
# Export passwords from 'pass' (the unix password manager) to a single text file

dest=pass_entries.txt

# Note: this requires bash 4.0+
shopt -s nullglob globstar

prefix=${PASSWORD_STORE_DIR:-$HOME/.password-store}

for file in "$prefix"/**/*.gpg; do
    file="${file/$prefix//}"
    printf "%s\n" "Name: ${file%.*}" >> $dest
    pass "${file%.*}" >> $dest
    printf "\n\n" >> $dest
done
