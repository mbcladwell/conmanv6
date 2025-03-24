#! /bin/bash
export LC_ALL="C"
guileexecutable -e '(conmanv5 fetch-unsubscribes)' -s conmanstorepath/share/guile/site/3.0/conmanv5/fetch-unsubscribes.scm

