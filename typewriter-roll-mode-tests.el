;;; typewriter-roll-mode-tests.el -- tests for typewriter-roll-mode

;;; Code:

(require 'ert)
(require 'typewriter-roll-mode)

(ert-deftest trm-backspace ()
  (should (not
           (let ((this-command "hello"))
             (typewriter-roll--is-backspace))))
  (should (let ((this-command 'delete-backward-char))
            (typewriter-roll--is-backspace))))

(provide 'typewriter-roll-mode-tests)

;;; typewriter-roll-mode-tests.el ends here
