;;; typewriter-roll-mode.el --- Aid for distraction-free writing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, line, carriage, writing, distraction, cr, rewind
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://github.com/KeyWeeUsr/typewriter-roll-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor mode attempts to remove distraction of seeing the previous lines
;; of text while dumping an uninterrupted stream of thoughts, hence preventing
;; focus jumping to revisions whether to handle typos, spacing or complete
;; rewording and losing such a thought in the process.

;;; Code:

(defsubst typewriter-roll-scroll-up ()
  "Scroll current line to the top."
  (recenter-top-bottom 0))

(defsubst typewriter-roll-scroll-or-nothing (pos)
  "Scroll if the cursor POS changed (content wrapped)."
  (unless (eq pos (current-column))
      (typewriter-roll-scroll-up)))

(defsubst typewriter-roll-scroll-main (pos)
  "Main function for checking the first line scrolled to the top.
Argument POS cursor's position."
  (progn
    (fill-paragraph)
    (typewriter-roll-scroll-or-nothing pos)))

(defsubst typewriter-roll-is-backspace ()
  "Check if current command is a backspace (left delete)."
  (eq this-command 'delete-backward-char))

(defun typewriter-roll-check ()
  "Check after typing whether to scroll up as in typewriter."
  (when (or (typewriter-roll-is-backspace) (eq last-command-event 32))
    (unless (eq (char-before) 32)
        (typewriter-roll-scroll-main (current-column))
      (typewriter-roll-scroll-up))))

(defun typewriter-roll-mode-activate ()
  "Activate TypewriterRoll locally to a buffer."
  (inline)
  ;; depth=nil, local to the current buffer=t
  (add-hook 'post-self-insert-hook #'typewriter-roll-check nil t)
  (add-hook 'pre-command-hook #'typewriter-roll-check nil t))

(defun typewriter-roll-mode-deactivate ()
  "Dectivate TypewriterRoll locally to a buffer."
  (inline)
  ;; local to the current buffer=t
  (remove-hook 'post-self-insert-hook #'typewriter-roll-check t)
  (remove-hook 'pre-command-hook #'typewriter-roll-check t))

(define-minor-mode typewriter-roll-mode
  "Minor mode to justify and scroll up text and keep only single
line in focus"
  :lighter " typewriter-roll"
  (if typewriter-roll-mode
      (typewriter-roll-mode-activate)
    (typewriter-roll-mode-deactivate)))

(provide 'typewriter-roll-mode)
;;; typewriter-roll-mode.el ends here
