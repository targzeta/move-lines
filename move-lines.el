;;; move-lines.el --- move current line or lines surrounding region up or down

;; Copyright (C) 2014 Emanuele Tomasi <targzeta@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;; Author: Emanuele Tomasi <targzeta@gmail.com>
;; Version: 1.0
;; URL: https://gist.github.com/4170263
;; Maintainer: Emanuele Tomasi <targzeta@gmail.com>
;; Keywords: convenience

;;; Commentary;

;; There are two entry points: `move-lines-up' moves the text up and
;; `move-lines-down' that moves the text down.
;;
;; Copy this file in a directory which is in the  Emacs `load-path'. Then,
;; execute the following code either directly or in your .emacs file:
;;
;;     (require 'move-lines)
;;     (move-lines-binding)
;;
;; Now, you can move the line(s) up by M-p or M-<up> or down by M-n or
;; M-<down>.

;;; Code:

(defun move-lines--internal (n)
  (let* ((start (point)) ;; The position of beginning of line of the first line
         (end start)     ;; The position of eol+\n of the end line
         col-init        ;; The current column for the first line
         (col-end (current-column)) ;; The current column for the end line
         exchange_pm     ;; If I had exchanged point and mark
         delete-latest-newline) ;; If I had inserted a newline at the end

    ;; STEP 1: Identifying the line(s) to cut.
    ;; ---
    ;; If region is actives, I ensure that point always is at the end of the
    ;; region and mark at the beginning.
    (when (region-active-p)
      (when (< (point) (mark))
        (setq exchange_pm t)
        (exchange-point-and-mark))
      (setq start (mark)
            end (point)
            col-end (current-column)))

    (goto-char start) (setq col-init (current-column))
    (beginning-of-line) (setq start (point))

    (goto-char end) (end-of-line)
    ;; If point == point-max, this buffers doesn't have the trailing newline.
    ;; In this case I have to insert a newline otherwise the following
    ;; `forward-char' (to keep the "\n") will fail.
    (when (= (point) (point-max))
      (setq delete-latest-newline t)
      (insert-char ?\n) (forward-char -1))
    (forward-char 1) (setq end (point))

    ;; STEP 2: Moving the lines.
    ;; ---
    ;; The region I'm cutting span from the beginning of line of the current
    ;; line (or current region) to the end of line + 1 (newline) of the current
    ;; line (or current region).
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      ;; If the current-column != 0, I have moved the region at the bottom of a
      ;; buffer doesn't have the trailing newline.
      (when (not (= (current-column) 0))
        (insert-char ?\n)
        (setq delete-latest-newline t))
      (setq start (+ (point) col-init)) ;; Now, start is the start of new region
      (insert line-text))

    ;; STEP 3: Restoring
    ;; ---
    ;; I'm at the end of new region (or line) and start has setted at the
    ;; beginning of new region (if a region is active).
    ;; Restoring the end column.
    (forward-line -1)
    (forward-char col-end)

    (when delete-latest-newline
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))

    (when (region-active-p)
      (setq deactivate-mark nil)
      (set-mark start)
      (if exchange_pm
          (exchange-point-and-mark)))))

;;;###autoload
(defun move-lines-up (n)
  "Moves the current line or, if region is actives, the lines surrounding
region, up by N lines, or 1 line if N is nil."
  (interactive "p")
  (if (eq n nil)
      (setq n 1))
  (move-lines--internal (- n)))

;;;###autoload
(defun move-lines-down (n)
  "Moves the current line or, if region is actives, the lines surrounding
region, down by N lines, or 1 line if N is nil."
  (interactive "p")
  (if (eq n nil)
      (setq n 1))
  (move-lines--internal n))

;;;###autoload
(defun move-lines-binding ()
  "Sets the default key binding for moving lines. M-p or M-<up> for moving up
and M-n or M-<down> for moving down."
  (global-set-key (kbd "M-p") 'move-lines-up)
  (global-set-key (kbd "M-<up>") 'move-lines-up)
  (global-set-key (kbd "M-n") 'move-lines-down)
  (global-set-key (kbd "M-<down>") 'move-lines-down))

(provide 'move-lines)

;; move-lines.el ends here
