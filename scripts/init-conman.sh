#! /bin/bash
export LC_ALL="C"
mkdir -p $HOME/conman/bak
mkdir -p $HOME/conman/tmp
touch $HOME/conman/envs.txt
echo "((\"bcc-recipient\" . \"mbc2025@labsolns.com\")(\"sender\" . \"mbc2025@labsolns.com\")(\"send-report-flag\" . #t))" >> $HOME/conman/envs.txt

