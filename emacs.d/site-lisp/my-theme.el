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

;; Changes the mode line colors for background.
(set-face-background 'modeline "black")
;; Changes the mode line colors for foreground.
(set-face-foreground 'modeline "orange")
;; Highlight color you see when you mark text with the mouse
(set-face-foreground 'region "white")

;;(set-background-color        "red") ; Set emacs bg color 
(set-foreground-color        "white") ; Set emacs fg color 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Detect the version of emacs that is running and operating system:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The version information for various Operating Systems by doing
;; "meta-x version on scratch buffer"
;;
;; Here is a list of version strings:
;; Fedora 14:
;; GNU Emacs 23.2.1 (x86_64-redhat-linux-gnu, GTK+ Version 2.21.4) of 2010-07-08 on x86-10.phx2.fedoraproject.org
;; RHEL 5:
;; GNU Emacs 21.4.1 (x86_64-redhat-linux-gnu, X toolkit, Xaw3d scroll bars) of 2011-04-07 on hs20-bc2-3.build.redhat.com
;; RHEL 6:
;; GNU Emacs 23.1.1 (x86_64-redhat-linux-gnu, GTK+ Version 2.18.9) of 2011-02-01 on x86-003.build.bos.redhat.com
;; OSX Lion 10.7.1
;; GNU Emacs 22.1.1 (mac-apple-darwin) of 2011-06-24 on doublemagnum.apple.com

;; Certain versions of emacs do not have all the functions like GNU
;; Emacs 21. We will detect the version and then load functions that
;; apply to each version so that we do not error out on loading.
(cond
 ((string-match "GNU Emacs 21" (emacs-version))
  (message "Detected that emacs is version: GNU Emacs 21.")
  ;; Put in version releated font preferences in here.
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
