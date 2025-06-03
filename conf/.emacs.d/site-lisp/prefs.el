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
;; Follow symlinks and don't ask.
(setq vc-follow-symlinks t)
;; Always end a file with a newline, t to enable
(setq require-final-newline t)
;; Wrap at col 70
(setq-default fill-column 80)
;; Show matching parenthesis
(show-paren-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabs and Spaces Preferences:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - http://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil)

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-guess-indent-offset t) ; python-indent-guess-indent-offset. When set to a non-nil value, it attempts to guess the indentation offset based on the existing indentation in the file.
            (setq indent-tabs-mode nil) ; Use spaces
            (setq python-indent 4)))   ; 4 spaces for indentation
