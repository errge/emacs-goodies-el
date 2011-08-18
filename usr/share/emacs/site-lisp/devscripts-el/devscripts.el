;; Routines to do devscripts-compatible emacs routines.
;; copyright 2002 Junichi Uekawa.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; readme-debian.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.

(require 'pbuilder-log-view-mode)
(require 'comint)

(defgroup devscripts nil "devscripts mode"
  :group 'tools
  :prefix "devscripts-")


(defcustom debuild-option-list '("-i" "-uc" "-us") "*Options to give to debuild."
  :type '(repeat string)
  :group 'devscripts)
(defconst devscripts-mode-version "$Id: devscripts.el,v 1.5 2007-07-13 15:13:30 dancer Exp $" "Version of devscripts mode.")

(defun devscripts-internal-get-debian-package-name ()
  "Find the directory with debian/ dir, and get the dir name."
  (let* ((looking-dir (expand-file-name (concat default-directory "."))))
    (while (not (file-accessible-directory-p (concat looking-dir "/debian")))
      (progn 
	(if (string= looking-dir "/")
	    (error "Cannot find debian dir anywhere"))
	(setq looking-dir (expand-file-name (expand-file-name (concat looking-dir "/.."))))))
    (file-name-nondirectory looking-dir)))

(defun debuild ()
  "Run debuild in the current directory."
  (interactive)
  (let* ((debuild-buffer (concat "*debuild*" default-directory))
	 (debuild-process (concat "debuild-process-" default-directory))
	 (package-name (devscripts-internal-get-debian-package-name)))
    (switch-to-buffer debuild-buffer)
    (toggle-read-only 0)
    (kill-region (point-min) (point-max))
    (compilation-mode)    
    (pbuilder-log-view-add package-name debuild-buffer (apply 'start-process debuild-process debuild-buffer "/usr/bin/debuild" debuild-option-list))))

(defun debi ()
  "Run debi in the current directory, to install debian packages generated by previous invocation of debuild."
  (interactive)
  (let* ((debi-name (concat "debi" default-directory))
	 (debi-buffer-name (concat "*" debi-name "*")))
    (make-comint debi-name devscripts-mode-gain-root-command
		 nil "/usr/bin/debi")
    (switch-to-buffer debi-buffer-name)))

(defun debit ()
  "Run debit in the current directory, to install debian packages generated by previous invocation of debuild."
  (interactive)
  (let* ((debit-buffer (concat "*debit*" default-directory))
	 (debit-process (concat "debit-process-" default-directory)))
    (switch-to-buffer debit-buffer)
    (kill-region (point-min) (point-max))
    (compilation-mode)
    (start-process debit-process debit-buffer devscripts-mode-gain-root-command "/usr/bin/debit")))


(defun debc ()
  "Run debc in the current directory, to install debian packages generated by previous invocation of debuild."
  (interactive)
  (let* ((debc-buffer (concat "*debc*" default-directory))
	 (debc-process (concat "debc-process-" default-directory)))
    (switch-to-buffer debc-buffer)
    (kill-region (point-min) (point-max))
    (devscripts-debc-mode)
    (start-process debc-process debc-buffer "/usr/bin/debc")))

(defun debclean ()
  "Run debclean in the current directory, to clean the debian build tree."
  (interactive)
  (let* ((debclean-buffer (concat "*debclean*" default-directory))
	 (debclean-process (concat "debclean-process-" default-directory)))
    (switch-to-buffer debclean-buffer)
    (kill-region (point-min) (point-max))
    (compilation-mode)
    (start-process debclean-process debclean-buffer "/usr/bin/debclean")))

(defun debdiff (changes-file-1 changes-file-2)
  "Compare contents of CHANGES-FILE-1 and CHANGES-FILE-2."
  (interactive "fFirst Changes file: \nfSecond Changes File: ")
  (let* ((debdiff-buffer (concat "*debdiff*" default-directory))
	 (debdiff-process (concat "debdiff-process-" default-directory)))
    (switch-to-buffer debdiff-buffer)
    (kill-region (point-min) (point-max))
    (start-process debdiff-process debdiff-buffer "/usr/bin/debdiff" 
		   (expand-file-name changes-file-1)
		   (expand-file-name changes-file-2))))

(defun debdiff-current ()
  "Compare the contents of .changes file of current version with previous version; 
requires access to debian/changelog, and being in debian/ dir."
  (interactive)
  (let* ((debdiff-buffer (concat "*debdiff*" default-directory))
	 (debdiff-process (concat "debdiff-process-" default-directory))
	 (debug-on-error t)
	 newversion oldversion pkgname changes-file-1 changes-file-2)
    (find-file "changelog")
    (save-excursion 
      (goto-char (point-min))
      (re-search-forward "^\\(\\S-+\\) +(\\([^:)]*:\\)?\\([^)]*\\))" nil t)
      (setq newversion (match-string 3))
      (setq pkgname (match-string 1))
      (re-search-forward "^\\(\\S-+\\) +(\\([^:)]*:\\)?\\([^)]*\\))" nil t)
      (setq oldversion (match-string 3)))
    (setq changes-file-1
	  (car (file-expand-wildcards (concat default-directory "../../" pkgname "_" oldversion "_*.changes"))))
    (setq changes-file-2
	  (car (file-expand-wildcards (concat default-directory "../../" pkgname "_" newversion "_*.changes"))))
    (princ pkgname)
    (princ oldversion)
    (princ changes-file-1)
    (princ changes-file-2)
    (switch-to-buffer debdiff-buffer)
    (kill-region (point-min) (point-max))
    (insert (concat 
	     "Comparing " 
	     (file-name-nondirectory changes-file-1) " and " 
	     (file-name-nondirectory changes-file-2)  "\n"))
    (start-process debdiff-process debdiff-buffer "/usr/bin/debdiff" 
		   (expand-file-name changes-file-1)
		   (expand-file-name changes-file-2))))

(defun devscripts-debc-mode ()
  "Mode to view debc output.
\\{devscripts-debc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'devscripts-debc-mode)
  (setq mode-name "debc")
  (mapcar 'make-local-variable '(font-lock-defaults))
  (use-local-map devscripts-debc-mode-map)
  (set-syntax-table devscripts-debc-mode-syntax-table)
  (setq font-lock-defaults 
	'(
					;keywords start here
	  (("^[a-z].*deb$" . font-lock-string-face)
	   ("^ \\([A-Z][-A-Za-z]+:\\)\\(.*\\)$" (1 font-lock-keyword-face) (2 font-lock-warning-face))
	   ("^[^ ].*$" . font-lock-comment-face)
	   )
	  nil		;keywords-only
	  nil		;case-fold
	  ()		;syntax-alist
	  ))
  (run-hooks 'devscripts-debc-mode-hook)
)

(defvar devscripts-debc-mode-map nil "Keymap for devscripts debc mode.")
(defvar devscripts-debc-mode-syntax-table nil "Syntax table for devscripts debc mode.")
(if devscripts-debc-mode-syntax-table
         ()              ; Do not change the table if it is already set up.
       (setq devscripts-debc-mode-syntax-table (make-syntax-table))
       (modify-syntax-entry ?\" ".   " devscripts-debc-mode-syntax-table)
       (modify-syntax-entry ?\\ ".   " devscripts-debc-mode-syntax-table)
       (modify-syntax-entry ?' "w   " devscripts-debc-mode-syntax-table))
(defcustom devscripts-mode-gain-root-command "/usr/bin/sudo" "*The command used to gain root for running debi and debit."
  :group 'devscripts
  :type 'file)
(defcustom devscripts-mode-load-hook nil "*Hooks that are run when devscripts-mode is loaded."
  :group 'devscripts
  :type 'hook)
(run-hooks 'devscripts-mode-load-hook)
(provide 'devscripts)
