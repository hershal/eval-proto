;;; eval-proto.el --- Evaluate interpreted languages inside Emacs

;; Copyright (C) 2017  Hershal Bhave

;; Author: Hershal Bhave <hershal.bhave@gmail.com>
;; Keywords: convenience, extensions, tools
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.   If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar eval-proto-mode-interpreter-alist
  '((js2-mode . "node") (ruby-mode . "ruby")
    (emacs-lisp-mode . "emacs -Q --batch")))

(defun eval-proto/get-interpreter ()
  "This function will return the interpreter of the current file
based on the current `major-mode'. The interpreter is retrieved
by the `major-mode' alist stored in
`eval-proto-mode-interpreter-alist'. Customize the alist in
`eval-proto-mode-interpreter-alist' to change the automatically
determined interpreter."
  (alist-get major-mode
             eval-proto-mode-interpreter-alist
             (eval-proto/get-shebang) t))

(defun eval-proto/get-shebang ()
  "Get the shell command to run current buffer based on the
shebang line."
  (save-excursion
    (goto-char 0)
    (let ((firstline (thing-at-point 'line)))
      (if (< (length firstline) 3) nil
        (substring
         (eval-proto/string-trim
          (replace-regexp-in-string
           "\\(/\\*\\)\\|\\(\\*/\\)\\|\\(//\\)" "" firstline))
         2)))))

(defun eval-proto/string-trim (str)
  "Return a string without leading and trailing whitespace. `STR'
is the input string to trim."
  (replace-regexp-in-string "^[[:space:]]*\\([^[:space:]]*\\)[[:space:]]$" "\1" str))

(defun eval-proto/eval (&optional prefix)
  "Evalute the current buffer or region with the buffer's
shebang, and print the result in the minibuffer. When given
`PREFIX', also push the results into the `kill-ring'."
  (interactive "P")
  (let* ((command (eval-proto/get-interpreter))
         (contents
          (eval-proto/eval-backend (concat "*eval-proto*") command)))
    (if (and command contents)
        (message contents)
      (progn (when prefix (kill-new contents))
             (message (concat "Could not determine executable command for this"
                              " buffer.  You can fix this by adding to"
                              "`eval-proto-mode-interpreter-alist'"))
             )
      )))


(defun eval-proto/eval-backend (buffer command)
  "Evaluate `BUFFER' with `COMMAND', and return the result."

  ;; delete the contents of `buffer'
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))))
  (let ((debug-on-error t) (start 1) (end 1))
    ;; If the mark is active, set the point and mark to the selected region;
    ;; else select the entire buffer.
    (cond
     (mark-active
      (setq start (point))
      (setq end (mark)))
     (t
      (setq start (point-min))
      (setq end (point-max))))

    ;; Send the input from `start' to `end' through stdin to the `process' with
    ;; arguments `args'.  This will popluate the `buffer' with the results.
    (shell-command-on-region
     start end     ; seems the order does not matter
     command
     buffer        ; the buffer to populate
     nil           ; no redisply during output
     )         ; the rest of the arguments
    (setq deactivate-mark t)
    (with-current-buffer buffer
      (buffer-string))))


(provide 'eval-proto)
;;; eval-proto.el ends here
