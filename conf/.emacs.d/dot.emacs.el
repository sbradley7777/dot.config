(message "Loading the Emacs configuration file: ~/.emacs.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management on Emacs v.24 or higher only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loading package management and MELPA repository.")
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; The init will load all the packages into the load path.
(package-initialize)
(add-to-list 'package-pinned-packages '("gnu-elpa-keyring-update" . "gnu"))
;; Install or update the key required for "melpa".
;;   - https://stackoverflow.com/questions/5701388/where-can-i-find-the-public-key-for-gnu-emacs
(unless (package-installed-p 'gnu-elpa-keyring-update)
  ;; Save default value of `package-check-signature' variable
  (defvar package-check-signature-default package-check-signature)
  ;; Disable signature checking
  (setq package-check-signature nil)
  ;; Download package archives (without signature checking)
  (package-refresh-contents)
  ;; Install package `gnu-elpa-keyring-update' (without signature checking)
  (package-install 'gnu-elpa-keyring-update t)
  ;; Restore `package-check-signature' value to default.
  (setq package-check-signature package-check-signature-default))
;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install and load packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; myPackages contains a list of package names
(defvar myPackages
  '(spacemacs-theme ;; Theme
    )
  )

;; Scans the list in myPackages and if the package listed is not already installed, then install it.
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; Customize theme.
;;   - https://melpa.org/#/?q=theme&sort=downloads&asc=false
;;   - https://github.com/nashamri/spacemacs-theme?tab=readme-ov-file#override-themes-colors
(custom-set-variables '(spacemacs-theme-custom-colors
      '(
        (bg1 . "#000000")
	)))
(load-theme 'spacemacs-dark t)

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

;; Load my personal configuration files.
(load-library "filearchive.el")
;; Load all the keyboard and hotkeys preferences.
(load-library "hotkeys.el")
;; Load all functions defined.
(load-library "functions.el")
;; Load all misc preferences.
(load-library "prefs.el")
;; Load all theme preferences. This is comment out because loading a packaged theme.
;; (load-library "theme.el")
;; Load all mode preferences.
(load-library "modes.el")

;; Load VIM modeline: https://github.com/cinsk/emacs-vim-modeline
(message "Loading emacs-vim-modeline.")
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI tweaks via emacs menu:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set location of any changes to emacs while running.
(setq custom-file "~/.emacs.d/site-lisp/custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last message before initialization is complete.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loading of the Emacs configuration file has completed.")
