
(defvar count-words-mode nil
  "Mode variable for \"Count Words\" mode")

(defvar count-words-mode-hook nil
  "Hooks to call on entering \"Count Words\" mode")

(defvar count-words-mode-map nil
  "Keymap for Count Words mode")

(unless count-words-mode-map
  (setq count-words-mode-map (make-sparse-keymap))
  (define-key count-words-mode-map "q" 'count-words-mode-quit))

(defun count-words-mode-quit ()
  (interactive)
  (kill-buffer))

(defun count-words-mode--setup ()
  (font-lock-mode)
  (erase-buffer)
  (use-local-map count-words-mode-map))

(defun count-words-mode ()
  "Display the results of 'find-words-frequency'

Special commands:

\\{count-words-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'count-words-mode)
  (setq mode-name "Count Words") ; TODO the mode should be Count Words Results mode
  (count-words-mode--setup)
  (run-hooks 'count-words-mode-hook))

(provide 'count-words-mode)
