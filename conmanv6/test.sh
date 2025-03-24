#! /bin/bash
export LC_ALL="C"
guile -L . -l "./conmanv5/cemail.scm" -e main -s "conmanv5.scm"
