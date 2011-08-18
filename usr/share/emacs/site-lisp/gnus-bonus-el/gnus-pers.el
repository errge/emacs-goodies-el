;;; gnus-pers.el --- implements personalities for gnus Message mode
;; Copyright (C) 1999 BrYan P. Johnson

;; Author: BrYan P. Johnson <bilko@onebabyzebra.com>
;; Keywords: news, mail, gnus

;; gnus-pers.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; gnus-pers.el is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; gnus-pers implements personalities for Message mode.
;;
;; It allows you to define a personality with any e-mail address, extra
;; headers and signature you like, either as strings, functions or variables
;; (signatures may also be files). Then it will chose a personality for you
;; based on header info, gnus-newsgroup-name or group
;; parameter. Additionally, you can change a personality in the middle of
;; writing a message. This is intended to be an alternative to
;; gnus-posting-styles, which only sets personalities (posting styles) based
;; on which group you are in. Also, gnus-posting-styles must be defined by
;; editing a your .gnus whereas gnus-pers uses Xemacs customize facility.

;; To use, add this to your .gnus file:
;;
;;  (require 'gnus-pers)
;;  (gnus-personality-init)
;;
;; Then setup and customize with:
;;
;;  M-x customize-group Personality'

;;; History:

; 1.0
; + Added check for personality group parameter. Use it to designate a
; particular personality for a particular group, with GP.
; + Added ability for values of From, Xtra headers Signature to be
; functions or variables as well as strings. Currently if you use a
; function for your xtra headers, it must return the header name as
; well.
; + Changed Customize to value menu to be more friendly

; 1.1
; + Added ability to specify signature as a file.
; + Fixed to use message-setup-hook rather than signature-setup-hook. Much nicer now. One bug, though. If doing a followup, can get extraneous Cc:.
; + Michael Livshin <mlivshin@bigfoot.com> : Changed electric headers to use nnmail-split-fancy.
; + Changed electric headers to use gnus-pers-split instead, and made variables customize-able.
; + Added gnus-personality-menu function to add a menubar entry if the buttonized from doesn't work.
; + "Paul D. Smith" <pausmith@nortelnetworks.com> : added gnus-personality-choose for choosing personalities via prompt. bound to C-c C-p in message mode
; + Fixed extraneous Cc: when doing followup bug. If you're getting your Cc: fields accidentally deleted, change gnus-pers-cc-fix to nil.
; + Stole message-insert-signature and changed a tad to fix the extra newline in signature when switching personalities bug. see gnus-pers-insert-signature
; + Added ability to use gnus-newsgroup-name as an electric criteria.

; 1.2 Peter S. Galbraith <psg@debian.org>
; + Use functionp instead of message-functionp since gnus no longer defines it.
; + Copy replace-in-string from XEmacs, name it
;   `gnus-personalities-replace-in-string' after removing the calls to
;   check-argument-type.  This was listed in the Todo list.

;; 1.3 Elias Oltmanns <oltmanns@uni-bonn.de>
;; Reported and fixed Debian bug #384209
;; `Cc-fix feature in gnus-pers is horribly broken'

;; 1.4  Bruce Stephens <bruce@cenderis.demon.co.uk> and
;;      Elias Oltmanns <oltmanns@uni-bonn.de>
;;
;; When in a group which has a personality setting (I set a general one in
;; my top-level topic) gnus-personality-choose doesn't allow me to change
;; personality. (Closes #263371)

;Todo:
; + redo x-tra headers to be a repeat list of two parts, header name
; and header data. Then allow either to be a function.
; + clean up.
; + Have Cc: fix actually compare e-mail addresses rather than futzing around with strings.
; + BBDB integration
; + Have from and extra headers possibly be files as well.
; + Electric rescan buffer

;;; Code:

(eval-when-compile (require 'cl))
(require 'nnmail)
(autoload 'rmail-dont-reply-to "mail-utils")
(autoload 'gnus-extract-address-components "gnus-util")

;; Variable setup

(defgroup Personality nil
  "Personalities for sending messages in Gnus."
  :group 'message)


(defcustom gnus-personalities nil
  "Personalities for gnus...
You may specify a function or variable name for the From field or the
Signature.  Gnus-Pers expects functions to return a string which
Gnus-Pers will insert into its proper place in the buffer (ie.  `fortune'
is an acceptable function as it returns a string, but
`message-insert-signature' isn't as it simply modifies the buffer.) To
add a newline to a field, use .  For example, to add two extra
headers, enter:

Reply-to: foo@bar.com X-My-Header:baz

To create a default personality, and avoid being prompted every time
there is no match, simply name one personality \"default\".  You may
set the group parameter \"personality\" to the name of a personality
you want to use every time you send mail from a group."


  :group 'Personality
  :type '(repeat (cons :tag "" (string :tag "Personality Name") (list :tag "	Personality Data" (choice :tag "		From" (string) (function) (variable))
								      (choice :tag "		Extra headers" (string) (function) (variable))
								      (choice :tag "		Signature" (string) (function) (variable)(file))))))



(defcustom gnus-personality-electric-headers nil
  "Determines which personality to use.
The symbol is one in `gnus-personality-split-abbrev-alist'.  This
works in a manner similar to `nnmail-split-fancy'."
  :group 'Personality
  :type '(repeat (list :format "%v"
		       (symbol :tag "Symbol to check")
		       (regexp :tag "Regular expression to match on")
		       (string :tag "Personality to use")))
)

(defcustom gnus-personality-split-abbrev-alist (cons '(group . ",gnus-newsgroup-name")
  (cons '(newsgroup . "newsgroups")
						     (cons `(ngroupto . ,(concat "newsgroups"
										(let ((to (cdr-safe (assq 'to nnmail-split-abbrev-alist))))
										  (and to (concat "\\|" to)))))
							   nnmail-split-abbrev-alist)))
  "*Alist of abbreviations allowed in `gnus-personality-electric-headers'."
  :group 'Personality
   :type '(repeat (cons :format "%v" symbol regexp)))

(defcustom gnus-personality-electric-to-headers nil
  "Which personality to use based on a regexp of the To: field.
This is deprecated.  Use `gnus-personality-electric-headers'."
  :group 'Personality
  :type '(repeat
	  (cons
	   (regexp :tag "Regular expression to match in the To: field")
	   (string :tag "Personality to use")))
  )

(defcustom gnus-personality-electric-ng-headers nil
  "Which personality to use based on a regexp of the Newsgroups:
field.  This is deprecated.  Use `gnus-personality-electric-headers'."
  :group 'Personality
  :type '(repeat
	  (cons
	   (regexp :tag "Regular expression to match in the Newsgroups: field")
	   (string :tag "Personality to use")))
  )

(defcustom gnus-pers-cc-fix t
  "Use the temporary Cc: field fix.
This only applies when doing followups when you should be doing a
reply.  Set this to nil if you're seeing disappearing Cc: fields."
  :group 'Personality
  :type 'boolean
)


;; Internal Variables


(defvar gnus-personality-menu-length 40
  "Maximum displayed length of an item in the menu."
  )


(defvar gnus-personality-last-used nil
  "The last gnus-personality used.
This is needed to clean up mail buffers when switching personalities.")

(defvar gnus-personality-split-trace nil)
(defvar gnus-personality-split-tracing nil)
(defvar gnus-personality-split-history nil)
(defvar gnus-personality-split-cache nil)

;; Functions.


(defun gnus-personality-init ()
  "Install Personality functionality into message mode."
  (interactive)
  (add-hook 'message-setup-hook
	    (lambda () (gnus-personality-electric-headers headers)
	      ))
  )

(defun gnus-personality-electric-headers (header-alist)
  "Function that chooses a personality based on headers.
Argument HEADER-ALIST an alist of headers passed from message-mode."
  (if gnus-personality-electric-headers
      (let ((split gnus-personality-electric-headers)
	    personality)
	(progn
	  (while (and (not personality) (car split))
	    (setq personality (gnus-pers-split (car split))
		  split (cdr split)))
	  (if personality
	      (gnus-personality-use (car personality))
	    (gnus-personality-use)
	    )
	  ))
    (progn
					; Bad bad bad. This must go away.
      (cond
       ((cdr-safe (assoc 'Newsgroups header-alist))
	(let* ((groups (cdr-safe (assq 'Newsgroups header-alist)))
	       (personality (gnus-personality-find gnus-personality-electric-ng-headers groups)))
	  (gnus-personality-use personality)))
       ((cdr-safe (assoc 'To header-alist))
	(let* ((groups (cdr-safe (assq 'To header-alist)))
	       (personality (gnus-personality-find gnus-personality-electric-to-headers groups)))
	  (gnus-personality-use personality)))
       (t
	(gnus-personality-use))))
    )
  )


(defun gnus-personality-find (speclist groups)
  "Deprecated."
  ;; This must go away too.
  (cond ((null speclist) nil)
	((null groups) (cdr-safe (assq t speclist)))
	(t
	 (let ((spec (car speclist)))
	   (if (or (eq t (car spec)) (string-match (car spec) groups))
	       (cdr spec)
	     (gnus-personality-find (cdr speclist) groups))))
        ))

(defun gnus-personality-prompt ()
  "Prompt for a personality to use with a message."
  (interactive "*P")
  (let ((testlist gnus-personalities)
	list-of-personalities temp temp2 personality)
    (progn
      (while (setq temp (pop testlist))
	(let ((temp2 (car temp)))
	  (push temp2 list-of-personalities)))
      (setq personality (read-from-minibuffer "Personality: " nil nil nil 'list-of-personalities))
      )
    (if (interactive-p)
	(insert personality))
    personality)
  )

(defun gnus-personality-popup-menu (args)
  "Personality popup menu."
  (interactive "e")
  (let ((response (get-popup-menu-response
		   `("Personalities"
		     :filter gnus-personality-menu-filter
		     "Select a personality to insert:"
		     "-----"
		     ))))
    (set-buffer (event-buffer event))
    (goto-char (event-point event))
    (funcall (event-function response) (event-object response))))

(defun gnus-personality-buttonize-from ()
  (goto-char (point-min))
  (search-forward "From:")
  (beginning-of-line)
  (let ((from (point))
	(to (+ (point) 5)))
    (gnus-article-add-button from to 'gnus-personality-popup-menu nil)))

(defun gnus-personality-menu ()
"Add to `message-mode-hook' if the Buttonized from doesn't work for you."

(add-submenu nil '("Pers" :filter gnus-personality-menu-filter "Select a personality to insert." "-----")))

(defun gnus-personality-menu-filter (menu-items)
  "Build the personality menu dynamically from all defined personalities."

  (let ((testlist gnus-personalities)
	list-of-personalities temp temp2 personality temp3 temp4)
    (progn
      (while (setq temp (pop testlist))
	(let ((temp2 (car temp)))
	  (push temp2 list-of-personalities)))
      (append menu-items
	      (mapcar
	       #'(lambda (temp3)
		   (if (> (length temp3) gnus-personality-menu-length)
		       (setq temp4 (substring temp3 0 gnus-personality-menu-length))
		     (setq temp4 temp3))
		   (vector temp4 `(gnus-personality-use ',temp3) t)) list-of-personalities)
	      )
      )))

(defun gnus-personality-choose (p)
  "Choose a personality for this message."
  (interactive
   (list (completing-read "Personality: " gnus-personalities nil 1)))
  (gnus-personality-use p))

(define-key message-mode-map "\C-c\C-p" 'gnus-personality-choose)

(defun gnus-personality-use (&optional personality)
  "Use a personality defined in gnus-personalities."
  (interactive)
					; first thing's first, shwick the signature.
  (goto-char (point-min))
  (if (re-search-forward message-signature-separator nil t)
      (progn
	(beginning-of-line)
	(let* ((beg (match-beginning 0))
	       (end (point-max)))
	  (delete-region beg end)
	  (goto-char (point-min)))))
					; Now let's see if there's already a personality installed.
  (goto-char (point-min))
  (if gnus-personality-last-used
      (let* ((values (assoc gnus-personality-last-used gnus-personalities))
	    (foo1 (cdr values))
	    (fromfoo (car foo1))
	    (from  (cond
		    ((stringp fromfoo)
		    fromfoo)
		   ((or (symbolp fromfoo)
			(functionp fromfoo))
		    (cond ((functionp fromfoo)
			   (funcall fromfoo))
			  ((boundp fromfoo)
			   (symbol-value fromfoo))))
		   ((listp fromfoo)
		    (eval fromfoo))
		    ))
	    (foo (cdr foo1))
	    (extrasfoo (car foo))
	    (extras  (cond
		    ((stringp extrasfoo)
		    extrasfoo)
		   ((or (symbolp extrasfoo)
			(functionp extrasfoo))
		    (cond ((functionp extrasfoo)
			   (funcall extrasfoo))
			  ((boundp extrasfoo)
			   (symbol-value extrasfoo))))
		   ((listp extrasfoo)
		    (eval extrasfoo))
		    ))
	     (fromlong (concat "From: "
			       from
			       "\n"))
	     (longextras (concat extras
				 "\n")))
	(progn
	  (if (search-forward fromlong nil t)
	      (if (not (equal fromlong "From: \n"))
		(let* ((end (match-end 0))
		       (start (match-beginning 0)))
		  (delete-region start end))))
	  (if (search-forward longextras nil t)
	      (if (not (equal longextras "\n"))
	      (let* ((end (match-end 0))
		     (start (match-beginning 0)))
		(delete-region start end)))
	    ))
	

	)
    )


  ; if group has a personality parameter, use it.
  (when (and (not personality) gnus-newsgroup-name)
    (let ((tmp-pers (gnus-group-find-parameter gnus-newsgroup-name 'personality t)))
      (when tmp-pers
	(setq personality tmp-pers))))


  (if (or personality (assoc "default" gnus-personalities))
      (let* ((values (assoc (or personality "default") gnus-personalities))
	     (foo1 (cdr values))
	     (fromfoo (car foo1))
	     (from  (cond
		     ((stringp fromfoo)
		      fromfoo)
		     ((or (symbolp fromfoo)
			  (functionp fromfoo))
		      (cond ((functionp fromfoo)
			     (funcall fromfoo))
			    ((boundp fromfoo)
			     (symbol-value fromfoo))))
		     ((listp fromfoo)
		      (eval fromfoo))
		     ))
	     (foo (cdr foo1))
	     (extrasfoo (car foo))
	     (extras  (cond
		       ((stringp extrasfoo)
			extrasfoo)
		       ((or (symbolp extrasfoo)
			    (functionp extrasfoo))
			(cond ((functionp extrasfoo)
			       (funcall extrasfoo))
			      ((boundp extrasfoo)
			       (symbol-value extrasfoo))))
		       ((listp extrasfoo)
			(eval extrasfoo))
		       ))
	     (foo2 (cdr foo))
	     (signaturesfoo (car foo2))
	     (signature  (cond
			  ((stringp signaturesfoo)
			   signaturesfoo)
			  ((or (symbolp signaturesfoo)
			       (functionp signaturesfoo))
			   (cond ((functionp signaturesfoo)
				  (funcall signaturesfoo))
				 ((boundp signaturesfoo)
				  (symbol-value signaturesfoo))))
			  ((listp signaturesfoo)
			   (eval signaturesfoo))
			  )))
		   
					;	     (from (cdr (assoc 'from values)))
					;	     (extras (cdr (assoc 'extras values)))
					;	     (signature (cdr (assoc 'signature values))))
	(progn
	  (if (and (not personality) (assoc "default" gnus-personalities))
	      (let ((personality "default"))
		(progn
		  
					; Set up the last-personality in case we change them:
		  (setq gnus-personality-last-used personality)
		  ))
	    (setq gnus-personality-last-used personality))
					;Let's do From: first.
	  (if (and from (not (equal from "")))
	      (progn
                (goto-char (point-min)) ;Make sure we're in the header
		(let ((endpos (search-forward mail-header-separator)))
		  (goto-char (point-min))
		  (if (re-search-forward "^From:" endpos t)
		      (progn
			(beginning-of-line)
			(let ((beg (point)))
			  (end-of-line)
			  (delete-region beg (point))
			  )
			(insert (concat
				 "From: "
				 from
				 )
				)
			)
		    (progn
		      (goto-char (point-min))
		      (insert (concat
			       "From: "
			       from
			       "\n"
			       )
			      )
		      )
		    ))))
	  (if gnus-pers-cc-fix
	  ; Now we have a problem with Cc when doing a followup. So let's check the Cc field and see if from is there:
	  (save-excursion
	    (save-restriction
	      (message-narrow-to-head)
	      (let ((case-fold-search t)
		    (rmail-dont-reply-to-names
		     (regexp-quote
		      (cadr (gnus-extract-address-components from))))
		    (cc (message-fetch-field "cc")))
		(when (and cc
			   (string-match rmail-dont-reply-to-names cc))
		  (message-remove-header "cc")
		  (widen)
		  (unless (string= "" (setq cc (rmail-dont-reply-to cc)))
		    (message-goto-cc)
		    (insert cc)))

	      )
	    )
	    )
)

					; Now the extra headers
	  (if (and extras (not (equal extras "")))
	      (progn
		(goto-char (point-min))
		(search-forward mail-header-separator)
		(forward-line -1)
		(end-of-line)
		(insert (concat
			 "\n"
			 extras
			 )
			)
	      
		)
	    )
	  
					; Now the signature
	  (if (and signature (not (equal signature "")))
	      (let ((oldsig message-signature)
		    (oldsigfile message-signature-file))
		(cond ((file-exists-p signature)
		       (setq message-signature-file signature))
		      (t
		       (setq message-signature signature)))
		(gnus-pers-insert-signature)
		(setq message-signature oldsig)
		(setq message-signature-file oldsigfile))
	    )
					; Now be nice and put point at the beginning of the message
	  (goto-char (point-min))
	  (search-forward mail-header-separator)
	  (forward-line 1)
	  (beginning-of-line)
	  )
	)
    
  
    
    
    (let* ( (personality (gnus-personality-prompt))
	    (values (assoc personality gnus-personalities))
					;	    (from (cdr (assoc 'from values)))
					;	    (extras (cdr (assoc 'extras values)))
					;	    (signature (cdr (assoc 'signature values))))
	    (foo1 (cdr values))
	    (fromfoo (car foo1))
	    (from  (cond
		    ((stringp fromfoo)
		    fromfoo)
		   ((or (symbolp fromfoo)
			(functionp fromfoo))
		    (cond ((functionp fromfoo)
			   (funcall fromfoo))
			  ((boundp fromfoo)
			   (symbol-value fromfoo))))
		   ((listp fromfoo)
		    (eval fromfoo))
		    ))
	    (foo (cdr foo1))
	    (extrasfoo (car foo))
	    (extras  (cond
		    ((stringp extrasfoo)
		    extrasfoo)
		   ((or (symbolp extrasfoo)
			(functionp extrasfoo))
		    (cond ((functionp extrasfoo)
			   (funcall extrasfoo))
			  ((boundp extrasfoo)
			   (symbol-value extrasfoo))))
		   ((listp extrasfoo)
		    (eval extrasfoo))
		    ))
	    (foo2 (cdr foo))
	    (signaturesfoo (car foo2))
	    (signature  (cond
			 ((stringp signaturesfoo)
			  signaturesfoo)
			 ((or (symbolp signaturesfoo)
			      (functionp signaturesfoo))
			  (cond ((functionp signaturesfoo)
				 (funcall signaturesfoo))
				((boundp signaturesfoo)
				 (symbol-value signaturesfoo))))
			 ((listp signaturesfoo)
			  (eval signaturesfoo))
			 )))
      (progn
					; Set up the last-personality in case we change them:
	(setq gnus-personality-last-used personality)
					;Let's do From: first.
	(if (and from (not (equal from "")))
	    (progn
	      (let ((endpos (search-forward mail-header-separator)))
		(goto-char (point-min))
		(if (re-search-forward "From:" endpos t)
		    (progn
		      (beginning-of-line)
		      (let ((beg (point)))
			(end-of-line)
			(delete-region beg (point))
			)
		      (insert (concat
			       "From: "
			       from
			       )
			      )
		      )
		  (progn
		    (goto-char (point-min))
		    (insert (concat
			     "From: "
			     from
			     "\n"
			     )
			    )
		    )
		  ))))
	  (if gnus-pers-cc-fix
	  ; Now we have a problem with Cc when doing a followup. So let's check the Cc field and see if from is there:
	  (save-excursion
	    (save-restriction
	      (message-narrow-to-head)
	      (let ((case-fold-search t)
		    (rmail-dont-reply-to-names
		     (regexp-quote
		      (cadr (gnus-extract-address-components from))))
		    (cc (message-fetch-field "cc")))
		(when (and cc
			   (string-match rmail-dont-reply-to-names cc))
		  (message-remove-header "cc")
		  (widen)
		  (unless (string= "" (setq cc (rmail-dont-reply-to cc)))
		    (message-goto-cc)
		    (insert cc)))

	      )
	    )
	    )
)
	    
					; Now the extra headers
	(if (and extras (not (equal extras "")))
	    (progn
	      (goto-char (point-min))
	      (search-forward mail-header-separator)
	      (forward-line -1)
	      (end-of-line)
	      (insert (concat
		       "\n"
		       extras
		       )
		      )
	      
	      )

	  )
					; Now the signature
	  (if (and signature (not (equal signature "")))
	       (let ((oldsig message-signature)
		     (oldsigfile message-signature-file))
		 (cond ((file-exists-p signature)
			(setq message-signature-file signature))
		       (t
			(setq message-signature signature)))
		(gnus-pers-insert-signature)
		(setq message-signature oldsig)
		(setq message-signature-file oldsigfile))
	    
)
	    )
			)
      )
       
					; buttonize from now that we're done
  (gnus-personality-buttonize-from)
			; Now be nice and put point at the beginning of the message

  (goto-char (point-min))
  (message-goto-body)

  )

(defun gnus-pers-insert-signature (&optional force)
"Gnus-Personalities Insert a signature.
See documentation for the `message-signature' variable."
  (interactive (list 0))
  (let* ((signature
	  (cond
	   ((and (null message-signature)
		 (eq force 0))
	    (save-excursion
	      (goto-char (point-max))
	      (not (re-search-backward message-signature-separator nil t))))
	   ((and (null message-signature)
		 force)
	    t)
	   ((functionp message-signature)
	    (funcall message-signature))
	   ((listp message-signature)
	    (eval message-signature))
	   (t message-signature)))
	 (signature
	  (cond ((stringp signature)
		 signature)
		((and (eq t signature)
		      message-signature-file
		      (file-exists-p message-signature-file))
		 signature))))
    (when signature
      (goto-char (point-max))
      ;; Insert the signature.
      (unless (bolp)
	(insert "\n"))
      (insert "-- \n")
      (if (eq signature t)
	  (insert-file-contents message-signature-file)
	(insert signature))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      )))

(defun gnus-pers-expand-newtext (newtext)
  (let ((len (length newtext))
	(pos 0)
	c expanded beg N did-expand)
    (while (< pos len)
      (setq beg pos)
      (while (and (< pos len)
		  (not (= (aref newtext pos) ?\\)))
	(setq pos (1+ pos)))
      (unless (= beg pos)
	(push (substring newtext beg pos) expanded))
      (when (< pos len)
	;; We hit a \; expand it.
	(setq did-expand t
	      pos (1+ pos)
	      c (aref newtext pos))
	(if (not (or (= c ?\&)
		     (and (>= c ?1)
			  (<= c ?9))))
	    ;; \ followed by some character we don't expand.
	    (push (char-to-string c) expanded)
	  ;; \& or \N
	  (if (= c ?\&)
	      (setq N 0)
	    (setq N (- c ?0)))
	  (when (match-beginning N)
	    (push (buffer-substring (match-beginning N) (match-end N))
		  expanded))))
      (setq pos (1+ pos)))
    (if did-expand
	(apply 'concat (nreverse expanded))
      newtext)))

(defun gnus-pers-split (split)
  ;; Return a list of groups matching SPLIT.
  (let (cached-pair)
    (cond
     ;; nil split
     ((null split)
      nil)

     ;; A group name.  Do the \& and \N subs into the string.
     ((stringp split)
      (when gnus-personality-split-tracing
	(push (format "\"%s\"" split) gnus-personality-split-trace))
      (list (gnus-pers-expand-newtext split)))

     ;; Junk the message.
     ((eq split 'junk)
      (when gnus-personality-split-tracing
	(push "junk" gnus-personality-split-trace))
      (list 'junk))

     ;; Check the cache for the regexp for this split.
     ((setq cached-pair (assq split gnus-personality-split-cache))
      (let (split-result
	    (end-point (point-max))
	    (value (nth 1 split)))
	(if (symbolp value)
	    (setq value (cdr (assq value gnus-personality-split-abbrev-alist))))
	(while (and (goto-char end-point)
		    (re-search-backward (cdr cached-pair) nil t))
	  (when gnus-personality-split-tracing
	    (push (cdr cached-pair) gnus-personality-split-trace))
	  (let ((split-rest (cddr split))
		(end (match-end 0))
		;; The searched regexp is \(\(FIELD\).*\)\(VALUE\).  So,
		;; start-of-value is the the point just before the
		;; beginning of the value, whereas after-header-name is
		;; the point just after the field name.
		(start-of-value (match-end 1))
		(after-header-name (match-end 2)))
	    ;; Start the next search just before the beginning of the
	    ;; VALUE match.
	    (setq end-point (1- start-of-value))
	    ;; Handle - RESTRICTs
	    (while (eq (car split-rest) '-)
	      ;; RESTRICT must start after-header-name and
	      ;; end after start-of-value, so that, for
	      ;; (any "foo" - "x-foo" "foo.list")
	      ;; we do not exclude foo.list just because
	      ;; the header is: ``To: x-foo, foo''
	      (goto-char end)
	      (if (and (re-search-backward (cadr split-rest)
					   after-header-name t)
		       (> (match-end 0) start-of-value))
		  (setq split-rest nil)
		(setq split-rest (cddr split-rest))))
	    (when split-rest
	      (goto-char end)
	      (let ((value (nth 1 split)))
		(if (symbolp value)
		    (setq value (cdr (assq value gnus-personality-split-abbrev-alist))))
		;; Someone might want to do a \N sub on this match, so get the
		;; correct match positions.
		(re-search-backward value start-of-value))
	      (dolist (sp (gnus-pers-split (car split-rest)))
		(unless (memq sp split-result)
		  (push sp split-result))))))
	split-result))

     ;; Not in cache, compute a regexp for the field/value pair.
     (t
      (let* ((field (nth 0 split))
	     (value (nth 1 split))
	     (retval (nthcdr 2 split))
	     partial regexp)
	;; Check to see if it's a "gnus-newsgroup-name" split
	(if (and gnus-newsgroup-name
		 (equal ",gnus-newsgroup-name" (cdr (assq field gnus-personality-split-abbrev-alist))))
	    (let ((groupname gnus-newsgroup-name))
	      (if (string-match value groupname)
		  retval))
	  (progn

	    (if (symbolp value)
		(setq value (cdr (assq value gnus-personality-split-abbrev-alist))))
	    (if (string= ".*" (substring value 0 2))
		(setq value (substring value 2)
		      partial ""))
	    (setq regexp (concat "^\\(\\("
				 (if (symbolp field)
				     (cdr (assq field gnus-personality-split-abbrev-alist))
				   field)
				 "\\):.*\\)"
				 (or partial "\\<")
				 "\\("
				 value
				 "\\)\\>"))
	    (push (cons split regexp) gnus-personality-split-cache)
	;; Now that it's in the cache, just call gnus-pers-split again
	;; on the same split, which will find it immediately in the cache.
	(gnus-pers-split split))
	  ))))))






(message "Loaded Gnus Personalities")

(provide 'gnus-pers)

;;; gnus-pers.el ends here
