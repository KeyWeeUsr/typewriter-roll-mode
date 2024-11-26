;;; typewriter-roll-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "typewriter-roll-mode" "typewriter-roll-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from typewriter-roll-mode.el

(autoload 'typewriter-roll-mode "typewriter-roll-mode" "\
Justify and scroll text to keep a single line in focus.

If called interactively, enable Typewriter-Roll mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "typewriter-roll-mode" '("typewriter-roll-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; typewriter-roll-mode-autoloads.el ends here
