;;;   SPDX-License-Identifier: GPL-3.0   -*- lexical-binding: t; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sat Jul 11 19:15:45 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
;;; patch-sclang-interp.el - patches the behaviour of
;;; `sclang-eval-sync' to work around elisp read errors, by supplying
;;; a replacement function for the command process-filter
;;; `sclang-command-process-filter'


(defun sclang-command-process-filter--read-invalid-read-syntax (string)
  "STRING should conform to the form of string which would be passed to
the `read' call in `sclang-command-process-filter'.  If the call to
elisp `read' would throw an `invalid-read-syntax' error, this hacky
function returns an sexp conforming to what `read' might have
returned, without calling actually `read' on the portion of the string
which would blown up the elisp reader, but instead returning that
portion as a literal string."
  (cl-assert (eql (elt string 0) ?\())
  (let* ((start 1))
    (cl-destructuring-bind (elt0 . len) (read-from-string string start)
      (setq start (1+ len))
      (cl-assert (= (elt string start) ?\())
      (incf start)
      (cl-assert (= (elt string start) ?\())
      (cl-destructuring-bind (time . len) (read-from-string string start)
	(cl-assert (= (elt string len) ?\ ))
	(setq start (1+ len))
	(cl-destructuring-bind (stat . len) (read-from-string string start)
	  (setq start (1+ len))
	  (let ((end (1- (length string))))
	    (cl-assert (= (elt string end) ?\)))
	    (let ((p (cl-position ?\  string :start start :end (1- end)
				  :from-end t)))
	      (cl-destructuring-bind (id . len)
		  (read-from-string string p)
		(cl-assert (= (elt string (1- p)) ?\)))
		(list elt0
		      (list time stat (substring string start (1- p))
			    id ))))))))))

(defvar sclang-command-process-filter--recover-from-elisp-read-errors t
  "If non-NIL change the default behaviour of
`sclang-command-process-filter' to try to hack your way out of any
elisp reader error.")

(defun sclang-command-process-filter (_proc string)
  "modified version of `sclang-command-process-filter' from
sclang-interp.el from Supercollider 3.13.0 sources.  This funcation
can catch and handle any `invalid-read-syntax' error that the elisp
reader might throw: if
`sclang-command-process-filter-recover-from-elisp-read-errors' is
non-NIL, and a read error occurs, instead of throwing the error this
function returns the portion of the STRING which caused the error as a
literal string."
  (when sclang-command-process-previous
    (setq string (concat sclang-command-process-previous string)))
  (let (end)
    (while (and (> (length string) 3)
		(>= (length string)
		    (setq end (+ 4 (sclang-string-to-int32 string)))))
      (if (not sclang-command-process-filter--recover-from-elisp-read-errors)
	  (sclang-handle-command-result
	   (read (decode-coding-string (substring string 4 end) 'utf-8)))
	(let ((dstr (decode-coding-string (substring string 4 end) 'utf-8)))
	  (sclang-handle-command-result
	   (condition-case _e
	       (read dstr)
	     (invalid-read-syntax
	      (sclang-command-process-filter--read-invalid-read-syntax dstr))))))
      (setq string (substring string end))))
  (setq sclang-command-process-previous string))

(when nil
  (setq $s "(evalSCLang ((1 2 3 4) ok #<class Integer>) nil)")
  (sclang-command-process-filter--read-invalid-read-syntax $s)
  (setq $s "(evalSCLang ((1 2 3 4) ok 3) nil)"))

(provide 'patch-sclang-interp)