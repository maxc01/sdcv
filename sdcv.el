;;; sdcv.el --- Emacs interface for sdcv             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Xingchen Ma
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

(defcustom sdcv-dict-backend 'sdcv
   "Specify which dictionary backend to use.
Possible values are:
- sdcv: Command line dictionary tool
- apple: macOS built-in dictionary"
  :type '(choice (const :tag "command line sdcv" sdcv)
                 (const :tag "apple dictionary" apple))
  :group 'sdcv)

(defvar sdcv-history
  (make-hash-table :test 'equal
                   :size 100)
  "A hash table of the previous search terms.")

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
                (completing-read
                 (propertize "Search word: " 'face '(:weight extra-bold))
                 sdcv-history)))))
    search-term))

(defun backend-sdcv (pattern)
  (let ((echo-message (ignore-error wrong-type-argument
                        (let* ((output (process-lines-ignore-status "sdcv" "-n" pattern))
                               (word (propertize (substring (nth 2 output) 3)
                                                 'face '(:foreground "#994639" :weight extra-bold)))
                               (phonetic (substring (nth 4 output) 1))
                               (definition (nth 5 output)))
                          (concat word " " phonetic " " definition)))))
    (if echo-message
        (progn
          (puthash pattern t sdcv-history)
          (message "%s" echo-message))
      (message "Did not find a definition for: %s" (propertize pattern 'face '(:weight extra-bold))))))

(defun backend-apple (pattern)
  (async-shell-command (format "open dict://%s" pattern)))

;;;###autoload
(defun sdcv-at-point (pattern)
  "Start a sdcv search for PATTERN."
  (interactive (list (sdcv--read-search-term)))
  (cond ((eq sdcv-dict-backend 'sdcv)
         (backend-sdcv pattern))
        ((eq sdcv-dict-backend 'apple)
         (backend-apple pattern))
        (t
         (message "Unknown backend"))))

(provide 'sdcv)
;;; sdcv.el ends here
