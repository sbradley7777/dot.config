;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redefined the background and foreground for black/green theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the background color and cursor type and color
(setq default-frame-alist
      '((cursor-color . "red")
	(cursor-type . box)
	(background-color . "black")))

(set-face-foreground 'bold-italic "Blue")

(custom-set-faces
 '(font-lock-comment-face
   ((((class color) (background light))
     :foreground "red")
    )))

(set-background-color "red")
(set-foreground-color "white")

;; Font for keywords, self, etc.
(set-face-foreground 'font-lock-keyword-face "MediumAquamarine")
;; Built in function or keywords like: len
(set-face-foreground 'font-lock-builtin-face "LightBlue")
;; Font for comments with #
(set-face-foreground 'font-lock-comment-face "Red" )
;; Font for anything in quotes
(set-face-foreground 'font-lock-string-face "Gold")
;; Not sure what this is for in python source code
(set-face-foreground 'font-lock-constant-face "Purple")
;; Font for the name of class definiations
(set-face-foreground 'font-lock-type-face "Pink")
;; Not sure what this is for in python source code
(set-face-foreground 'font-lock-warning-face "Red")
;; Font for function names.
(set-face-foreground 'font-lock-function-name-face "Yellow")
;; Font for global variable
(set-face-foreground 'font-lock-variable-name-face "Green")
(custom-set-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load font lock mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock mode for syntax highlight
(require 'font-lock)
;; Color the code
(global-font-lock-mode 1)
;; Use colours in font lock mode
(setq font-lock-maximum-decoration t)
;; Trun off limit on font lock mode
(setq font-lock-maximum-size nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change diff mode colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-foreground 'diff-header "purple")
  (set-face-background 'diff-header "blue")
  (set-face-foreground 'diff-file-header "purple")
  (set-face-background 'diff-file-header "blue")
  (set-face-foreground 'diff-index "green2")
  (set-face-background 'diff-index "blue")
  (set-face-foreground 'diff-added "green2")
  (set-face-background 'diff-added "blue")
  (set-face-foreground 'diff-removed "red2")
  (set-face-background 'diff-removed "blue")
  (set-face-foreground 'diff-changed "purple")
  (message "Setting diff mode colors." emacs-version))
(eval-after-load "diff-mode" '(custom-diff-colors))
