(setq load-path (cons "." load-path))
(require 'count-words-mode)

(defconst fwf--buffer-name "Words frequency")

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
            (fwf--update-word (downcase x) table))
          (fwf--all-words from to))
    table))

(defun fwf--word-length (pair)
  (length (cadr pair)))

(defun fwf--sorted-values (table)
  (sort
   (sort (fwf--unsorted-values table)
         (lambda (a b)
           (< (fwf--word-length b)
              (fwf--word-length a))))
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

(defun fwf--colorize (text color)
  (put-text-property 0 (length text) 'font-lock-face (list ':foreground color) text)
  text)

(defun fwf--bar (number)
  (let ((result ""))
    (while (> number 0)
      (setq result (concat result "*"))
      (setq number (- number 1)))
    (fwf--colorize result "red")))

(defun fwf--format-results (results &optional word-padding)
  (let* ((padding (or word-padding 0))
        (format-string (format "%%-%ds : %%s" padding)))
    (mapcar (lambda (pair)
              (format format-string (cadr pair) (fwf--bar (car pair))))
            results)))

(defun fwf--longest-word (results)
  (car
   (sort (mapcar 'cadr results)
         (lambda (a b) (> (length a)
                          (length b))))))

(defun fwf--printable-sorted-words (from to)
  (let* ((sorted-results (fwf--sorted-buffer-words from to)))
    (fwf--format-results sorted-results
                         (length (fwf--longest-word sorted-results)))))

(defun find-words-frequency ()
  "Print the frequency of each word in the current buffer and print them in a separate buffer"
  (interactive)
  (let ((results (fwf--printable-sorted-words (point-min) (point-max)))
        (buffer (get-buffer-create fwf--buffer-name)))
    (with-current-buffer buffer
      (font-lock-mode)
      (erase-buffer)
      (mapc (lambda (x)
              (insert x)
              (insert "\n"))
            results))
    (display-buffer-other-frame buffer)))
