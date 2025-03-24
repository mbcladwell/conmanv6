#! /bin/bash
export LC_ALL="C"
export PATH="/gnu/store/1gd9nsy4cps8fnrd1avkc9l01l7ywiai-guile-3.0.9/bin${PATH:+:}$PATH"
export GUILE_LOAD_PATH="/home/mbc/projects/conmanv5:/gnu/store/f626jha3digz643d51qbl9axdd0yvnk3-guile-gnutls-4.0.0/share/guile/site/3.0:/gnu/store/s09lrl5b3rd1v74l9szv5a8j17ihw4j4-gnutls-3.8.3/share/guile/site/3.0:/gnu/store/68s7665nmia10ks45fs514k835mlrskc-guile-dbi-2.1.8/share/guile/site/2.2:/gnu/store/gv71lwqlns6p4qa0jz82djpzxavvb85f-guile-gcrypt-0.4.0/share/guile/site/3.0${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH"
export GUILE_DBD_PATH="/gnu/store/8alvs4nwb64vkyqd746rfzqknadv2x2q-guile-dbd-mysql-2.1.8/lib"
guile -e main -s "./conmanv5.scm"
