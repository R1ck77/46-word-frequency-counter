

(defun find-words-frequency ()
  "Print the frequency of each word in the current region in a separate buffer"
  (interactive)
  (message (format "%s" (fwf-all-words (region-beginning) (region-end)))))

(defun update-word (word hash-table)
  (puthash word
           (+ (gethash word hash-table 0) 1)
           hash-table))

(defun fwf-all-words (from to)
  "Return a list of all words in the range specified

Some edge cases are not handled properly, but it's outside the scope of the exercise

e.g.

If a region starting at <B> and ending at <E> is defined like:

    badger <B>badger <E>mushroom

the word mushroom is gonna be counted."
  (let ((results (list)))
    (save-excursion
      (goto-char from)
      (while (< (point) to)
        (right-word)
        (setq results (cons (thing-at-point 'word) results))))
    results))


