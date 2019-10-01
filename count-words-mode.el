
(defvar count-words-mode nil
  "Mode variable for \"Count Words\" mode")

(if (not (assq 'count-words-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(count-words-mode " CountWords")
                minor-mode-alist)))

(defun count-words-mode--compute-mode-var (arg old-value)
  (if (null arg)
      (not old-value)
    (> (prefix-numeric-value arg) 0)))

(defun count-words-mode--turn-on ()
  (message "* Turning Count words mode ON"))

(defun count-words-mode--turn-off ()
  (message "* Turning Count words mode OFF"))

(defun count-words-mode (&optional arg)
  "Count Words buffer mode"
  (interactive "P")
  (let ((next-var-mode (count-words-mode--compute-mode-var arg count-words-mode)))
    (if next-var-mode
        (count-words-mode--turn-on)
      (count-words-mode--turn-off))
   (setq count-words-mode next-var-mode)))

(provide 'count-words-mode)
