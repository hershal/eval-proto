;;; eval-proto.el --- Evaluate interpreted languages inside Emacs

;; Copyright (C) 2017  Hershal Bhave

;; Author: Hershal Bhave <hershal.bhave@gmail.com>
;; Keywords: convenience, extensions, tools

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

;;

;;; Code:

;; if it can't find the version of the language you're using in this mapping,
;; look for the shebang.
(setq eval-proto-mode-interpreter-mapping
      '((js2-mode . ("node" "")) (ruby . ("ruby" ""))
        (emacs-lisp-mode . ("something" ""))))

(defun eval-proto/get-interpreter ()
  (alist-get major-mode
             eval-proto-mode-interpreter-mapping
             (eval-proto/get-shebang) t))

(defun eval-proto/get-shebang ()
  (save-excursion
    (goto-char 0)
    (let ((str (thing-at-point 'line t)))
      (if (or (< (length str) 3)
              (not (string-equal (substring str 0 2) "#!")))
          nil
        (let ((interpreter-and-list
               (split-string (substring str 2 -1) " ")))
          (list (car interpreter-and-list)
                (mapconcat 'identity (cdr interpreter-and-list) " ")))))))


(defun eval-proto/eval (&optional prefix interpreter args)
  "Evalute the current buffer (or region if mark-active), and
print the result in the message buffer. When given a prefix
argument, also push the results into the kill-ring."
  (interactive "P")
  (if-let ((interpreter-and-args
            (cond
             ((eval-proto/get-shebang)
              (eval-proto/get-shebang))
             ((eval-proto/get-interpreter)
              (eval-proto/get-interpreter))
             )))
      (let ((contents
             (eval-proto/eval-backend
              (concat "*eval-proto*")
              (car interpreter-and-args)
              (cadr interpreter-and-args))))
        (when prefix (kill-new contents))
        (message "%s" contents))
    (message "Could not determine interpreter executable for this buffer")))


(defun eval-proto/eval-backend (buffer process args)
  "Evaluate the current buffer (or region if mark-active), and
return the result"
  ;; delete the contents of `buffer`
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

    ;; Send the input from `start` to `end` through stdin to the `process` with
    ;; arguments `args`. This will popluate the `buffer` with the results.
    (shell-command-on-region
     start end     ; seems the order does not matter
     (string-join (append (list process) (split-string args " ")) " ")
     buffer        ; the buffer to populate
     nil           ; no redisply during output
     )         ; the rest of the arguments
    (setq deactivate-mark t)
    (with-current-buffer buffer
      (buffer-string))))


(provide 'eval-proto)
;;; eval-proto.el ends here
