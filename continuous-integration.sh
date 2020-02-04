#!/bin/bash

ARGS=$(getopt -o 'f' --long 'fast' -- "$@") || exit
eval "set -- $ARGS"

while true; do
    case $1 in
      (-f|--fast)
            ((FAST++)); shift;;
      (--)  shift; break;;
      (*)   exit 1;;           # error
    esac
done
remaining=("$@")

set -ev

cd `dirname $0`

git pull

if [[ $FAST ]];
then
  R CMD INSTALL .
else
  R CMD INSTALL . --preclean --debug
fi

cd server
set +v
source .venv/bin/activate
set -v
pip install --requirement requirements.txt

systemctl reload destinie
systemctl status destinie
