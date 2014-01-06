;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Misc Prefrences:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the time
(display-time)
;; Dont show the GNU splash screen
(setq inhibit-startup-message t)
;; Make searches case insenstive
(setq case-fold-search t)
;; Current line & column number of cursor in the mode line
(line-number-mode 1)
;; Add column numbers
(setq column-number-mode t)
;; Don't word wrap long lines
(set-default 'truncate-lines t)
;; Turn off jumpy scroll
(setq-default scroll-step 1)
;; No beeps, flash on errors
(setq-default visible-bell t)
;; Visual feedback on
(setq-default transient-mark-mode t)
;; The ctrl-k kills whole line if at col 0
(setq-default kill-whole-line t)
;; Highlights trailing whitespaces
(setq-default show-trailing-whitespace t)
;; Set title
(setq-default frame-title-format (list "My Emacs %b: %f"))

;; Turn off read only mode with .patch files
(setq diff-default-read-only nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line Preferences:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Always end a file with a newline, t to enable
(setq require-final-newline t)
;; Wrap at col 70
(setq-default fill-column 80)
;; Show matching parenthesis
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabs and Spaces Preferences:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python does not like spaces and tabs mixed together. There is some tools that
;; can help with this if you encounter some werid tab issue.
;;
;; Python prefers spacing over tabs.
;;
;; Use built in tool to search for bad tabbing.
;; $ python -m tabnanny ~/somefile.py
;;
;; There is a tool in most python packages for reindenting python files and
;; there is also one on github as well that can be searched for. The tool is
;; called: reindent.py. If not on system, then download from here:
;; - http://svn.python.org/projects/python/trunk/Tools/scripts/reindent.py
;;
;; Be careful because it will default to recursize:
;; $ reindent.py ~/somefile.py

;; Show tabs as 4 spaces
(setq-default tab-width 4)
;; No tabs, just spaces are used
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load slang mode support for event scripts in clustering:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'slang-mode "slang-mode"
  "Mode for editing slang source files")
(setq auto-mode-alist
      (append '(("\\.sl$" . slang-mode)) auto-mode-alist))


