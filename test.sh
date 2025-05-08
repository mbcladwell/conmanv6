#! /bin/bash
export LC_ALL="C"
export PATH="/gnu/store/1gd9nsy4cps8fnrd1avkc9l01l7ywiai-guile-3.0.9/bin${PATH:+:}$PATH"
export GUILE_LOAD_PATH="/home/mbc/projects/conmanv6:/gnu/store/hacg6cx3fpywgw271q16hpx9nirv7aga-guile-gnutls-4.0./share/guile/site/3.0:/gnu/store/659a0167j4fzg6a9h1s8p2v9bq2zcrwc-guile-dbi-2.1.8/share/guile/site/2.2:/gnu/store/gv71lwqlns6p4qa0jz82djpzxavvb85f-guile-gcrypt-0.4.0/share/guile/site/3.0"
export GUILE_DBD_PATH="/gnu/store/1cszcjr1bn0wlkvxvqc8gxdbfq5s95lv-guile-dbd-mysql-2.1.8/lib"
guile -e main -s "./conmanv6.scm"
