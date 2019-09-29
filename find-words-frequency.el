

(defun find-words-frequency ()
  "Print the frequency of each word in the current region in a separate buffer"
  (interactive)
  (message (format "%s" (fwf--all-words (region-beginning) (region-end)))))

(defun fwf--update-word (word hash-table)
  (puthash word
           (+ (gethash word hash-table 0) 1)
           hash-table))

(defun fwf--all-words (from to)
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
        (let ((word (thing-at-point 'word)))
          (if word
              (setq results (cons word results))))))
    results))

(defun fwf--count-words (from to)
  (lexical-let ((table (make-hash-table :test 'equal)))
    (mapc (lambda (x)
            (fwf--update-word x table))
          (fwf--all-words from to))
    table))

(defun fwf--sorted-values (table)
  (sort
   (fwf--unsorted-values table)
   (lambda (a b)
     (> (car a) (car b)))))

(defun fwf--unsorted-values (table)
  (let ((acc '()))
    (maphash (lambda (key value)
               (setq acc (cons (list value key) acc)))
             table)
    acc))

(defun fwf--sorted-buffer-words (from to)
  (fwf--sorted-values
   (fwf--count-words from to)))

(defun fwf--print-results (table)
  (mapc (lambda (tuple)
          (message (format "%s %s" (car tuple) (cadr tuple))))
        (fwf--sorted-values table)))
