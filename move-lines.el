;;; move-lines.el --- move current line or lines surrounding region up or down
;;
;; Copyright (C) 2014-2017 Emanuele Tomasi <targzeta@gmail.com>
;;
;; Author: Emanuele Tomasi <targzeta@gmail.com>
;; URL: https://github.com/targzeta/move-lines
;; Maintainer: Emanuele Tomasi <targzeta@gmail.com>
;; Keywords: convenience
;; Version: 2.0
;;
;; This file is NOT part of GNU Emacs.
;;
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
;;
;;; Commentary;
;;
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
;;
;;; Code:

(defun move-lines--internal (n)
  "Moves the current line or, if region is actives, the lines surrounding
region, of N lines. Down if N is positive, up if is negative"

  ;; The text area spans from the beginning of the first line (text-start) to
  ;; the end of the last line, '\n' included (text-end). Its coordinates are
  ;; the number of chars from the beginning of buffer.
  ;; The region is within the text area and its coordinates are the (negative)
  ;; numbers of chars from text-end.
  ;;
  ;; E.g.:
  ;;     Lorem ipsum dolor sit amet, consectetur adipisci elit,\n
  ;;     ^                  ^                    ^              ^
  ;; text-start(1)    region-start(-35)    region-end(-14)  text-end(55)
  ;;
  ;; We assume that point is always ahead the mark, else temporarily we swap
  ;; them.
  ;; If we act on the latest line of the buffer and it hasn't a newline, we
  ;; temporarily add one.
  (let* (text-start
         text-end
         (region-start (point))
         (region-end region-start)
         swap-point-mark
         delete-latest-newline)

    ;; STEP 1: identifying the text to cut.
    (when (region-active-p)
      (if (> (point) (mark))
          (setq region-start (mark))
        (exchange-point-and-mark)
        (setq swap-point-mark t
              region-end (point))))

    ;; text-end and region-end
    (end-of-line)
    ;; If point !< point-max, this buffers doesn't have the trailing newline.
    (if (< (point) (point-max))
        (forward-char 1)
      (setq delete-latest-newline t)
      (insert-char ?\n))
    (setq text-end (point)
          region-end (- region-end text-end))

    ;; text-start and region-start
    (goto-char region-start)
    (beginning-of-line)
    (setq text-start (point)
          region-start (- region-start text-end))

    ;; STEP 2: cut and paste.
    (let ((text (delete-and-extract-region text-start text-end)))
      (forward-line n)
      ;; If the current-column != 0, I have moved the region at the bottom of a
      ;; buffer doesn't have the trailing newline.
      (when (not (= (current-column) 0))
        (insert-char ?\n)
        (setq delete-latest-newline t))
      (insert text))

    ;; STEP 3: Restoring.
    (forward-char region-end)

    (when delete-latest-newline
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))

    (when (region-active-p)
      (setq deactivate-mark nil)
      (set-mark (+ (point) (- region-start region-end)))
      (if swap-point-mark
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
