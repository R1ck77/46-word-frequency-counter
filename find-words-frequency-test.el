(require 'buttercup)

(setq load-path (cons "." load-path))
(require 'find-words-frequency)

(describe "find-words-frequency.el"
  (describe "fwf--clean-string"
    (it "removes non-space non-alphanumeric characters"
      (expect (fwf--clean-string "foo!123 ;;bár b,az")
              :to-equal "foo123 bár baz"))
    (it "removes multiple consecutive spaces after cleaning non alnum characters"
      (expect (fwf--clean-string "foo!1  2  3 ;;bár   b,az")
              :to-equal "foo1 2 3 bár baz"))
    (it "turns all letters to lowercase"
      (expect (fwf--clean-string "AltolÁ")
              :to-equal "altolá"))))

