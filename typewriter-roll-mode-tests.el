;;; typewriter-roll-mode-tests.el -- tests for typewriter-roll-mode

;;; Code:

(require 'ert)
(require 'typewriter-roll-mode)

(defun typewriter-roll--cursor-line ()
  (line-number-at-pos))
(defun typewriter-roll--top-line ()
  (line-number-at-pos (window-start)))

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
      (switch-to-buffer (current-buffer))
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

(provide 'typewriter-roll-mode-tests)

;;; typewriter-roll-mode-tests.el ends here
