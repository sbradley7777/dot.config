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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Detect the version of emacs that is running:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Certain versions of emacs do not have all the functions like GNU
;; Emacs 21. We will detect the version and then load functions that
;; apply to each version so that we do not error out on loading.
(cond
 ((< emacs-major-version 22)
  (message "Detected that emacs is version: %s." emacs-version)
  ;; Put in version releated font preferences in here.
  (setq
   font-lock-face-attributes
   '((font-lock-comment-face "Red" nil nil t nil)
     (font-lock-string-face "Gold" nil nil t nil)
     (font-lock-keyword-face "MediumAquamarine")
     (font-lock-function-name-face "Yellow")
     (font-lock-variable-name-face "Green")
     (font-lock-type-face "Pink")
     (font-lock-reference-face "Red" nil nil t nil))
   font-lock-maximum-decoration t)
   ;;(let (new-face)
   ;;  (setq new-face (copy-face 'italic 'firebrick-italic))
   ;;  (set-face-foreground new-face "firebrick")
   ;;  (setq new-face (copy-face 'bold 'blue))
   ;;  (set-face-foreground new-face "blue")
   ;;  (setq new-face (copy-face 'bold 'royalblue))
   ;;  (set-face-foreground new-face "royalblue")
   ;;  (setq new-face (copy-face 'bold 'yellow))
   ;;  (set-face-foreground new-face "yellow")
   ;;  (setq new-face (copy-face 'bold 'firebrick))
   ;;  (set-face-foreground new-face "firebrick")
   ;;  (setq
   ;;   font-lock-comment-face 'firebrick-italic
   ;;   font-lock-string-face 'italic
   ;;   font-lock-keyword-face 'blue
   ;;   font-lock-function-name-face 'yellow
   ;;   font-lock-variable-name-face 'black
   ;;   font-lock-type-face 'royalblue
   ;;   font-lock-reference-face 'firebrick)))
  )
 (t (message "Detected that emacs is not version: GNU Emacs 21.")
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
    )
)

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
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))
