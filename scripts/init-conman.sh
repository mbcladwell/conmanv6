#! /bin/bash
export LC_ALL="C"
mkdir -p $HOME/conman/bak
mkdir -p $HOME/conman/tmp
echo "((\"bcc-email\" . #t)(\"send-report\" . #t))" >> $HOME/conman/envs.txt

