Default emacs-goodies-el package in el-get is seriously outdated and
buggy.  I made this repository by simply unpacking all the
architecture independent Debian binary packages related to
emacs-goodies-el.

I use it like this:

    (add-to-list 'el-get-sources
                 '(:name emacs-goodies-el
                         :type git
                         :url "https://github.com/errge/emacs-goodies-el.git"
                         :after (lambda ()
                                  (global-set-key [end]  'home-end-end)
                                  (global-set-key [home] 'home-end-home))
                         :load-path ("usr/share/emacs/site-lisp/debian-el"
                                     "usr/share/emacs/site-lisp/devscripts-el"
                                     "usr/share/emacs/site-lisp/dpkg-dev-el"
                                     "usr/share/emacs/site-lisp/emacs-goodies-el"
                                     "usr/share/emacs/site-lisp/gnus-bonus-el"
                                     "usr/share/emacs/site-lisp/vm-bonus-el"))
                 t)
    
