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
;;(setq-default tab-width 4)
;; No tabs, just spaces are used
;;(setq-default indent-tabs-mode nil)

;; Article where the tab functions are found:
;; http://www.emacswiki.org/emacs/NoTabs
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(setq indent-tabs-mode nil)
(infer-indentation-style)

(add-hook 'python-mode-hook
    (lambda ()
        (setq indent-tabs-mode nil)
        (infer-indentation-style)))

