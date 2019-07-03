;;; sdcv.el --- Emacs interface for sdcv             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xingchen Ma

;; Author: Xingchen Ma <maxc01@yahoo.com>
;; Keywords: sdcv

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar sdcv-history
  nil
  "A list of the previous search terms.")

(defun sdcv--read-search-term ()
  "Read a search term from the minibuffer.
If region is active, return that immediately.  Otherwise, prompt
for a string, offering the current word as a default."
  (let (search-term)
    (if (use-region-p)
        (progn
          (setq search-term
                (buffer-substring-no-properties (region-beginning) (region-end)))
          (deactivate-mark))
      (let* ((word (thing-at-point 'word t)))
        (if word
            (setq search-term word)
          (setq search-term
                (read-from-minibuffer "Search word: " nil nil nil 'sdcv-history nil)))
        ))
    (unless (equal (car sdcv-history) search-term)
      (push search-term sdcv-history))
    search-term))

(defun sdcv-at-point (word)
  "Start a sdcv search for WORD."
  (interactive (list (sdcv--read-search-term)))
  (let* ((output (process-lines "sdcv" "-n" word))
         (ww-from (propertize (substring (nth 2 output) 3)
                              'face '(:foreground "#994639" :weight extra-bold)
                              ))
         (phonetic (substring (nth 4 output) 1))
         (ww-to (nth 5 output))
         (echo-message (concat ww-from " " phonetic " " ww-to))
         )
    (message "%s" echo-message)
    )
  )

(provide 'sdcv)
;;; sdcv.el ends here




