
(defvar count-words-mode nil
  "Mode variable for \"Count Words\" mode")

(if (not (assq 'count-words-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(count-words-mode " CountWords")
                minor-mode-alist)))

(defvar count-words-mode-hook nil
  "Hooks to call on entering \"Count Words\" mode")

(defun count-words-mode--setup ()
  (font-lock-mode)
  (erase-buffer))

(defun count-words-mode ()
  "Count Words buffer mode"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'count-words-mode)
  (setq mode-name "Count Words") ; TODO the mode should be Count Words Results mode
  (count-words-mode--setup)
  (run-hooks 'count-words-mode-hook))

(provide 'count-words-mode)
