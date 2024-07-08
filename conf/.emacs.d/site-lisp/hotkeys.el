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


;; Spell check.
(global-set-key [f1] 'ispell-word)
;; Spell check the buffer.
(global-set-key [f2] 'ispell-buffer)

;; Kill the buffer
(global-set-key [f4] 'kill-this-buffer)
;; Copy what is highlighted
(global-set-key [f5] 'clipboard-kill-ring-save)
;; Delete all the extra whitespaces at end of line
(global-set-key [f6] 'delete-trailing-whitespace)

;; Keyboard commands for controlling the buffer.
;;
;; Scroll buffer down.
(global-set-key (kbd "ESC <left>") 'scroll-down)
;; Scroll buffer up
(global-set-key (kbd "ESC <right>") 'scroll-up)
;; Cycle to previous buffer.
(global-set-key [f7] 'previous-buffer)
;; Cycle to next buffer.
(global-set-key [f8] 'next-buffer)
;; Goto top of buffer.
(global-set-key [f9] 'beginning-of-buffer)
;; Goto end of buffer.
(global-set-key [f10] 'end-of-buffer)

;; Page down/up move the point, not the screen.  In practice, this means that
;; they can move the point to the beginning or end of the buffer.
;; http://snarfed.org/emacs_page_up_page_down
;; Page down the buffer.
(global-set-key [f11]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))
;; Page up the buffer.
(global-set-key [f12]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

