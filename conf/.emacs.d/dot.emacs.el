(message "Loading the Emacs configuration file: ~/.emacs.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management on Emacs v.24 or higher only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (> emacs-major-version 23)
  ;; http://melpa.milkbox.net/#/getting-started
  (message "Loading package management and MELPA repository.")
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  ;; The init will load all the packages into the load path.
  (package-initialize)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI tweaks via emacs menu:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set location of any changes to emacs while running.
(setq custom-file "~/.emacs.d/site-lisp/my-custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all the libraries under the directory: ~/.emacs.d/site-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add the site-lisp directory to load path
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Do a loop to automatically load all the files under that directory.
;;(loop for lib in (directory-files "~/.emacs.d/site-lisp/" 't "elc?$" 't) do
;;      (load-library lib))
;;(loop for lib in (directory-files "~/.emacs.d/site-lisp/" 't "\.elc?$" 't) do
;;      (message lib))
;;      (load-library lib))

;; Load my personal configuration files
(load-library "my-filearchive.el")
;; Load all the keyboard and hotkeys preferences
(load-library "my-hotkeys.el")
;; Load all functions defined.
(load-library "my-functions.el")
;; Load all misc preferences
(load-library "my-prefs.el")
;; Load all misc preferences
(load-library "my-theme.el")

(when (> emacs-major-version 22)
  ;; The htmlize lib is used for producing html pages out of buffer and only
  ;; works on emacs 23 or above.
  (require 'htmlize)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last Message before Initialization is Complete:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loading of the Emacs configuration file has completed.")
