(define-module (conmanv5)
#:use-module (conmanv5 env)  
#:use-module (conmanv5 utilities)
#:use-module (conmanv5 recs)
#:use-module (conmanv5 munger)
#:use-module (conmanv5 pubmed)
#:use-module (conmanv5 cemail)
#:use-module (srfi srfi-19)   ;; date time
#:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
#:use-module (ice-9 pretty-print)
#:export (main)
)

;;#! /bin/bash

;;source /home/admin/.guix-profile/etc/profile
;;export PATH=/home/admin/conmanv5/bin:${PATH:+:}$PATH
;;export GUILE_LOAD_PATH=/gnu/store/5yvzilh78996627i8avq532sl2c03i95-gnutls-3.6.15/share/guile/site/3.0:/home/admin/conmanv5:${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH
;;/gnu/store/m5iprcg6pb5ch86r9agmqwd8v6kp7999-guile-3.0.5/bin/guile -e '(conmanv5)' -s /home/admin/conmanv5/conmanv5.scm blah

;;2025-01-30 use guile -e '(conmanv5)' -L . -s ./conmanv5.scm from project directory
;; 3 2 * * * /home/admin/.guix-profile/bin/conman.sh --file=/home/admin/.msmtprc >/home/admin/conman.log 2>&1

;; guix shell --manifest=/home/mbc/projects/autostall/conmanv5/manifest.scm -- guile -e '(conmanv5)' -L . -s ./conmanv5.scm

;;on aws from ~/conmanv5 directory
;; guile -e '(conmanv5)' -L . -s ./conmanv5.scm

(define (main args)
  ;; args: '( "script name" "past days to query" "Number of articles to pull")
  ;; 2023-10-01 revision requires single arg that is not used
  (let* ((start-time (current-time time-monotonic))
	 ;;	 (a (get-summaries (cadr args) (caddr args)))
	 (a (get-summaries days-ago max-arts))
	 (dummy (map retrieve-article a))  ;;this does all the work; comment out last line for testing
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (/ (time-second (time-difference stop-time start-time)) 60)))
	 (stats-list (get-stats-list elapsed-time))
	 (dummy7 (send-report stats-list ))
	 )
;;   (pretty-print a)
    (pretty-print (string-append "Elapsed time: " (number->string  elapsed-time) " minutes." ))
   ;; #f
    ))
   

;;WORKFLOW

;;1 pubmed/get-summaries returns a list of (("37658855")  ("Jirangkul P"   "Lorsuwannarat N"   "Wanichjaroen N")) and
;;  also as side effect makes ref-records

;;2 retrieve-article
;;     -get the article from "https://pubmed.ncbi.nlm.nih.gov/" pmid "/"
;;     - fast: munger/get-author-records extracts authors from article; also extracts affiliations
;;
;;

;; /home/ubuntu/conman/bin/conman.sh >/home/ubuntu/conman/conman.log 2>&1
