(define-module (conmanv6)
#:use-module (conmanv6 env)  
#:use-module (conmanv6 utilities)
#:use-module (conmanv6 recs)
#:use-module (conmanv6 munger)
#:use-module (conmanv6 pubmed)
#:use-module (conmanv6 cemail)
#:use-module (conmanv6 sqlstuff)
#:use-module (srfi srfi-19)   ;; date time
#:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
#:use-module (ice-9 pretty-print)

#:export (main)
)

;;#! /bin/bash

;;source /home/admin/.guix-profile/etc/profile
;;export PATH=/home/admin/conmanv6/bin:${PATH:+:}$PATH
;;export GUILE_LOAD_PATH=/gnu/store/5yvzilh78996627i8avq532sl2c03i95-gnutls-3.6.15/share/guile/site/3.0:/home/admin/conmanv6:${GUILE_LOAD_PATH:+:}$GUILE_LOAD_PATH
;;/gnu/store/m5iprcg6pb5ch86r9agmqwd8v6kp7999-guile-3.0.5/bin/guile -e '(conmanv6)' -s /home/admin/conmanv6/conmanv6.scm blah

;;2025-01-30 use guile -e '(conmanv6)' -L . -s ./conmanv6.scm from project directory
;; 3 2 * * * /home/admin/.guix-profile/bin/conman.sh --file=/home/admin/.msmtprc >/home/admin/conman.log 2>&1

;; guix shell --manifest=/home/mbc/projects/autostall/conmanv6/manifest.scm -- guile -e '(conmanv6)' -L /home/mbc/projects/conmanv6 -s ./conmanv6.scm

;;on aws from ~/conmanv6 directory
;; guile -e '(conmanv6)' -L . -L /home/mbc/projects/conmanv6 -s ./conmanv6.scm


(define (first-four lst results counter)
  (if (or (null? (cdr lst)) (= counter 3))
      (begin
	(set! results (cons (car lst) results))
	results)
      (begin
	(set! results (cons (car lst) results))
	(set! counter (+ 1 counter))
	(first-four (cdr lst) results counter))
  ))


(define (main args)
  ;; args: '( "script name" "past days to query" "Number of articles to pull")
  ;; 2023-10-01 revision requires single arg that is not used
  (let* ((start-time (current-time time-monotonic))
	 (_ (get-envs))
	 ;;	 (a (get-summaries (cadr args) (caddr args)))
	 (a (get-summaries days-ago max-arts))
;;	 (a (first-four a '() 0))
	 (dummy (map retrieve-article a))  ;;this does all the work; comment out last line for testing
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (/ (time-second (time-difference stop-time start-time)) 60)))
	 (stats-list (get-stats-list elapsed-time (get-total-emails-sent)))
	 (dummy7 (if send-report (send-report stats-list )))
	 )
    (begin
      (update-conmanstats stats-list)
      ;;   (pretty-print a)
      (pretty-print (string-append "Elapsed time: " (number->string  elapsed-time) " minutes." ))
      (system "aws-ec2-shutdown.sh")  ;;works with crontab -e (not sudo); must set up asw-cli credentials file 
      ;; #f
      )))
   

;;WORKFLOW

;;1 pubmed/get-summaries returns a list of (("37658855")  ("Jirangkul P"   "Lorsuwannarat N"   "Wanichjaroen N")) and
;;  also as side effect makes ref-records

;;2 retrieve-article
;;     -get the article from "https://pubmed.ncbi.nlm.nih.gov/" pmid "/"
;;     - fast: munger/get-author-records extracts authors from article; also extracts affiliations
;;
;;

;; /home/ubuntu/conman/bin/conman.sh >/home/ubuntu/conman/conman.log 2>&1

;; DELETE FROM affils;
;; DELETE FROM conman;
;; DELETE FROM conmanstats;
;; DELETE FROM ref;
