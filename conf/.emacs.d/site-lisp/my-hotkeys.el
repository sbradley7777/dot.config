;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard bahavior and Custom HotKeys (aka: keyboard-modifiers or shortcuts):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kill the buffer
(global-set-key [f3] 'kill-this-buffer)
;; Copy what is highlighted
(global-set-key [f4] 'clipboard-kill-ring-save)
;; Delete all the extra whitespaces at end of line
(global-set-key [f6] 'delete-trailing-whitespace)

;; Cycle to previous buffer
(global-set-key [f8] 'previous-buffer)
;; Cycle to next buffer
(global-set-key [f9] 'next-buffer)

;; Set up the keyboard so the delete key on both the regular
;; keyboard and the keypad delete the character under the cursor
;; and to the right under X, instead of the default, backspacea
;; behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(setq delete-key-deletes-forward t)

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
;; http://snarfed.org/emacs_page_up_page_down

;; page up
(global-set-key [f12]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))
;; page down
(global-set-key [f10]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

(global-set-key [f11]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))


