(message "Loading the Emacs configuration file: ~/.emacs.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management on Emacs v.24 or higher only.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loading package management and MELPA repository.")
(require 'cl)
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
;;   - https://github.com/nashamri/spacemacs-theme
;;   - https://github.com/jorgenschaefer/elpy?tab=readme-ov-file
(defvar myPackages
  '(spacemacs-theme ;; Theme.
    elpy
    company         ;; Used by elpy.
    yaml-mode
    )
  )

;; Scans the list in myPackages and if the package listed is not already installed, then install it.
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure the theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   - https://melpa.org/#/?q=theme&sort=downloads&asc=false
;;   - https://github.com/nashamri/spacemacs-theme?tab=readme-ov-file#override-themes-colors
(custom-set-variables '(spacemacs-theme-custom-colors
      '(
        (bg1 . "#000000")
	)))
(custom-set-variables
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-custom-colors (quote ((bg1 . "#000000")))))
(custom-set-faces
 )
;; Load the theme.
(load-theme 'spacemacs-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable elpy.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq python-shell-interpreter "/usr/bin/python3")
(setq elpy-rpc-python-command "/usr/bin/python3")
(elpy-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all the libraries under the directory: ~/.emacs.d/site-lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add the site-lisp directory to load path
(setq custom-libraries (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path custom-libraries)

;; Do a loop to automatically load all the files under that directory.
(loop for filename in (directory-files custom-libraries 't "\.elc?$" 't) do
      (load-library (file-name-base filename)))

;; Removed  https://github.com/cinsk/emacs-vim-modeline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI tweaks via emacs menu:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set location of any changes to emacs while running. These changes are not
;; loaded when emacs restarts.
(setq custom-file "~/.emacs.d/site-lisp/custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last message before initialization is complete.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Loading of the Emacs configuration file has completed.")
