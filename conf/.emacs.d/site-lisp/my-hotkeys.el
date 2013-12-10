;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard bahavior and Custom HotKeys (aka: keyboard-modifiers or shortcuts):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List of hotkeys:  http://www.math.uh.edu/~bgb/emacs_keys.html
;; CTRL-h k to describe keys (describe-key).

;; Set up the keyboard so the delete key on both the regular keyboard and the
;; keypad delete the character under the cursor and to the right under X,
;; instead of the default, backspacea behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(setq delete-key-deletes-forward t)

;; Kill the buffer
(global-set-key [f3] 'kill-this-buffer)
;; Copy what is highlighted
(global-set-key [f4] 'clipboard-kill-ring-save)
;; Delete all the extra whitespaces at end of line
(global-set-key [f6] 'delete-trailing-whitespace)

;; Cycle to previous buffer
(global-set-key [f7] 'previous-buffer)
;; Cycle to next buffer
(global-set-key [f8] 'next-buffer)

;; Top of buffer
(global-set-key (kbd "ESC <up>") 'beginning-of-buffer)
;; Bottom of buffer
(global-set-key (kbd "ESC <down>") 'end-of-buffer)

;; Top of buffer
(global-set-key (kbd "ESC <left>") 'scroll-down)
;; Bottom of buffer
(global-set-key (kbd "ESC <right>") 'scroll-up)

;; Page down/up move the point, not the screen.  In practice, this means that
;; they can move the point to the beginning or end of the buffer.
;; http://snarfed.org/emacs_page_up_page_down
;; page down
(global-set-key [f11]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))
;; page up
(global-set-key [f12]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

