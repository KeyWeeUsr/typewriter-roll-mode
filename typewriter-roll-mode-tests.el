;;; typewriter-roll-mode-tests.el -- tests for typewriter-roll-mode

;;; Code:

(require 'ert)
(require 'typewriter-roll-mode)

(defun typewriter-roll--cursor-line ()
  (line-number-at-pos))
(defun typewriter-roll--top-line ()
  (line-number-at-pos (window-start)))
(defun typewriter-roll--fix-ert-window ()
  ;; (error "‘recenter’ing a window that does not display current-buffer.")
  (switch-to-buffer (current-buffer)))

(ert-deftest trm-backspace ()
  (should (not
           (let ((this-command "hello"))
             (typewriter-roll--is-backspace))))
  (should (let ((this-command 'delete-backward-char))
            (typewriter-roll--is-backspace))))

(ert-deftest normal-behavior ()
  "Write something until it overflows `fill-column', then scroll it away."
  (let ((scroll-margin 0)
        (typewriter-roll-keep-in-focus 0))
    (with-temp-buffer
      (typewriter-roll--fix-ert-window)
      (let* ((old-win-start (window-start))
             (old-win-end (window-end))
             (old-text "text text")
             (expected-text (format "%s\ntext" old-text)))
        ;; initial text
        (insert old-text)
        ;; default state
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (typewriter-roll--scroll-main (current-column))
          ;; default state, not enough chars to fill `fill-column'
          (should (string= (buffer-string) old-text))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          (insert " text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; only after
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2))

          (insert " ")  ; will be removed by filling
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2)))))))

(ert-deftest bug-1-prefer-keep-in-focus-zero ()
  "https://github.com/KeyWeeUsr/typewriter-roll-mode/issues/1

`typewriter-roll-keep-in-focus' is zero, `scroll-margin' is
ignored and zero. The cursor should move to the next line and
that line should be the only one visible on top."
  (let ((scroll-margin 0)
        (typewriter-roll-keep-in-focus 0)
        (typewriter-roll-prefer-scroll-margin nil))
    (with-temp-buffer
      (typewriter-roll--fix-ert-window)
      (let* ((old-win-start (window-start))
             (old-win-end (window-end))
             (old-text "text text")
             (expected-text (format "%s\ntext" old-text)))
        ;; initial text
        (insert old-text)

        ;; default state, cursor on line 1, top is line 1
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (typewriter-roll--scroll-main (current-column))
          ;; default state, not enough chars to fill `fill-column'
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) old-text))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert " text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, move top to line 2
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2))

          ;; space will be removed by filling, keep previous state
          (insert " ")
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2)))))))

(ert-deftest bug-1-prefer-keep-in-focus-non-zero ()
  "https://github.com/KeyWeeUsr/typewriter-roll-mode/issues/1

`typewriter-roll-keep-in-focus' is non-zero, `scroll-margin' is
ignored and zero. The cursor should move to the next line, but
keep one more line visible above."
  (let ((scroll-margin 0)
        (typewriter-roll-keep-in-focus 1)
        (typewriter-roll-prefer-scroll-margin nil))
    (with-temp-buffer
      (typewriter-roll--fix-ert-window)
      (let* ((old-win-start (window-start))
             (old-win-end (window-end))
             (old-text "text text")
             (expected-text (format "%s\ntext" old-text)))
        ;; initial text
        (insert old-text)

        ;; default state, cursor on line 1, top is line 1
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (typewriter-roll--scroll-main (current-column))
          ;; default state, not enough chars to fill `fill-column'
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) old-text))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert " text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, keep top on line 1
          ;; (i.e. +1 previous in focus)
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 1))

          ;; space will be removed by filling, keep previous state
          (insert " ")
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 1)))))))

(ert-deftest bug-1-prefer-scroll-margin-non-zero ()
  "https://github.com/KeyWeeUsr/typewriter-roll-mode/issues/1

`typewriter-roll-keep-in-focus' is zero, `scroll-margin' is
preferred and non-zero. The cursor should move to the next line,
but keep one more line visible above."
  (let ((scroll-margin 1)
        (typewriter-roll-keep-in-focus 0)
        (typewriter-roll-prefer-scroll-margin t))
    (with-temp-buffer
      (typewriter-roll--fix-ert-window)
      (let* ((old-win-start (window-start))
             (old-win-end (window-end))
             (old-text "text text")
             (expected-text (format "%s\ntext" old-text)))
        ;; initial text
        (insert old-text)

        ;; default state, cursor on line 1, top is line 1
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (typewriter-roll--scroll-main (current-column))
          ;; default state, not enough chars to fill `fill-column'
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) old-text))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert " text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, keep top on line 1
          ;; (i.e. +1 previous in focus)
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 1))

          ;; space will be removed by filling, keep previous state
          (insert " ")
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 1)))))))

(ert-deftest bug-1-prefer-scroll-margin-zero ()
  "https://github.com/KeyWeeUsr/typewriter-roll-mode/issues/1

`typewriter-roll-keep-in-focus' is zero, `scroll-margin' is
preferred and zero. The cursor should move to the next line and
that line should be the only one visible on top."
  (let ((scroll-margin 0)
        (typewriter-roll-keep-in-focus 0)
        (typewriter-roll-prefer-scroll-margin t))
    (with-temp-buffer
      (typewriter-roll--fix-ert-window)
      (let* ((old-win-start (window-start))
             (old-win-end (window-end))
             (old-text "text text")
             (expected-text (format "%s\ntext" old-text)))
        ;; initial text
        (insert old-text)

        ;; default state, cursor on line 1, top is line 1
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (typewriter-roll--scroll-main (current-column))
          ;; default state, not enough chars to fill `fill-column'
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) old-text))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert " text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, move top to line 2
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2))

          ;; space will be removed by filling, keep previous state
          (insert " ")
          (typewriter-roll--scroll-main (current-column))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2)))))))

(provide 'typewriter-roll-mode-tests)

;;; typewriter-roll-mode-tests.el ends here
