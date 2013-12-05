;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dont litter my os with autosaves which are files with ~ and backup
;; files which are files with #. The preferences are from:
;; http://snarfed.org/gnu_emacs_backup_files
;;
;; This link is what I am currently using because does not create
;; ~/.emacs_archive/auto-saves-list directory
;; http://stackoverflow.com/questions/2020941/emacs-newbie-how-can-i-hide-the-buffer-files-that-emacs-creates
;;
;; The autosave files will be located in this directory:
;;  ~/.emacs_archive/autosaves/
;; The backup files will be located in this directory:
;;  ~/.emacs_archive/backups/
;;
;; The directories will be automatically created if they do not exist.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stamp time on all files saved.
(require 'time-stamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put autosave files (example: #foo#) in ~/.emacs_archive/autosaves/:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar autosave-dir (expand-file-name "~/.emacs_archive/autosaves/"))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
;; Create the autosave directory if it does not exist.
(make-directory autosave-dir t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put backup files (example: foo~) in ~/.emacs_archive/:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar backup-dir (expand-file-name "~/.emacs_archive/backups/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
;; Create the backups directory if it does not exist.
(make-directory backup-dir t)

;; Various backup preferences.
(setq make-backup-files t)
(setq backup-by-copying t)
(setq backup-by-copying-when-mismatch t)
(setq backup-by-copying-when-linked t)
(setq version-control t)
;; Remove any backups that are either not the 2 oldest copies or the 3
;; newest copies.
(setq-default delete-old-versions t)
;; Keeps at most 3 copies that are newer than the 2 oldest
;; copies. This means there could be 5 total backups of a file at one
;; time.
(setq kept-new-versions 3)
;; Keeps two old copies that will not be deleted.
(setq kept-old-versions 2)

