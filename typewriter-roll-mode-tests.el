;;; typewriter-roll-mode-tests.el -- tests for typewriter-roll-mode

;;; Commentary:

;;; Code:

(require 'ert)
(require 'typewriter-roll-mode)

(defun typewriter-roll--cursor-line ()
  "Current line of cursor."
  (line-number-at-pos))
(defun typewriter-roll--top-line ()
  "Current line on the top of the buffer."
  (line-number-at-pos (window-start)))
(defun typewriter-roll--fix-ert-window ()
  "Fix \"‘recenter’ing a window that does not display current-buffer.\"."
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
             (expected-text (format "%s\ntext " old-text)))
        ;; initial text
        (insert old-text)

        ;; default state
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (execute-kbd-macro (kbd "SPC"))
          ;; default state, SPC does not trigger the roll
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) (format "%s " old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert "text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, move top to line 2
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2))

          ;; another space does not trigger the roll
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) (format "%s " expected-text)))
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
             (expected-text (format "%s\ntext " old-text)))
        ;; initial text
        (insert old-text)

        ;; default state, cursor on line 1, top is line 1
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (execute-kbd-macro (kbd "SPC"))
          ;; default state, SPC does not trigger the roll
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) (format "%s " old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert "text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, move top to line 2
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2))

          ;; another space does not trigger the roll
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) (format "%s " expected-text)))
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
             (expected-text (format "%s\ntext " old-text)))
        ;; initial text
        (insert old-text)

        ;; default state, cursor on line 1, top is line 1
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (execute-kbd-macro (kbd "SPC"))
          ;; default state, SPC does not trigger the roll
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) (format "%s " old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert "text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, keep top on line 1
          ;; (i.e. +1 previous in focus)
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 1))

          ;; another space does not trigger the roll
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) (format "%s " expected-text)))
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
             (expected-text (format "%s\ntext " old-text)))
        ;; initial text
        (insert old-text)

        ;; default state, cursor on line 1, top is line 1
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (execute-kbd-macro (kbd "SPC"))
          ;; default state, SPC does not trigger the roll
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) (format "%s " old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert "text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, keep top on line 1
          ;; (i.e. +1 previous in focus)
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 1))

          ;; another space does not trigger the roll
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) (format "%s " expected-text)))
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
             (expected-text (format "%s\ntext " old-text)))
        ;; initial text
        (insert old-text)

        ;; default state, cursor on line 1, top is line 1
        (should (string= (buffer-string) old-text))
        (should (eq (typewriter-roll--cursor-line) 1))
        (should (eq (typewriter-roll--top-line) 1))

        (let ((fill-column 10))
          (typewriter-roll-mode)

          (execute-kbd-macro (kbd "SPC"))
          ;; default state, SPC does not trigger the roll
          ;; keep cursor on line 1, keep top on line 1
          (should (string= (buffer-string) (format "%s " old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; text is inserted, --scroll-main not called yet
          (insert "text")
          (should (string= (buffer-string) (format "%s text" old-text)))
          (should (eq (typewriter-roll--cursor-line) 1))
          (should (eq (typewriter-roll--top-line) 1))

          ;; after --scroll-main call
          ;; move cursor to line 2, move top to line 2
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) expected-text))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2))

          ;; another space does not trigger the roll
          (execute-kbd-macro (kbd "SPC"))
          (should (string= (buffer-string) (format "%s " expected-text)))
          (should (eq (typewriter-roll--cursor-line) 2))
          (should (eq (typewriter-roll--top-line) 2)))))))

(ert-deftest bug-1-prefer-scroll-margin-branch ()
  "https://github.com/KeyWeeUsr/typewriter-roll-mode/issues/1

Test scrolling value used based on `typewriter-roll-prefer-scroll-margin'."
  (let ((scroll-margin 123)
        (typewriter-roll-keep-in-focus 456)
        result)
    (unwind-protect
        (progn
          (advice-add 'recenter-top-bottom
                      :override
                      (lambda (arg) (push arg result)))
          (dolist (item '(nil t))
            (let ((typewriter-roll-prefer-scroll-margin item))
              (with-temp-buffer
                (typewriter-roll--fix-ert-window)
                (typewriter-roll-mode)
                (insert "1\n2")
                (typewriter-roll--scroll-main (current-column)))))
          (should (equal (reverse result)
                         `(,typewriter-roll-keep-in-focus ,scroll-margin))))
      (advice-remove 'recenter-top-bottom
                     (lambda (arg) (push arg result))))))

(provide 'typewriter-roll-mode-tests)

;;; typewriter-roll-mode-tests.el ends here
