;;; winddown.el --- emacs port of vscode winddown -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-winddown
;; Version: 0.01
;; Package-Requires: ((emacs "26.3"))

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

;; Emacs port of VScode extension winddown

;;; Code:

(require 'cl-lib)
(require 'color)

(defgroup winddown nil
  "winddown"
  :group 'util)

(defcustom winddown-work-minutes 25
  "work time"
  :type 'integer)

(defface winddown--dummy
  '((((class color) (background light))
     (:background "red"))
    (((class color) (background dark))
     (:background "blue")))
  "Don't use this face")

(defvar winddown--update-interval 10.0)
(defvar winddown--face-info (make-hash-table :test #'equal))
(defvar winddown--timer nil)
(defvar winddown--update-times 0)

(defsubst winddown--light-p ()
  (string= (face-background 'winddown--dummy) "red"))

(defun winddown--update-color (face-info times)
  (let* ((rgb (plist-get face-info :rgb))
         (increments (plist-get face-info :increments))
         (r (cl-first rgb))
         (g (cl-second rgb))
         (b (cl-third rgb))
         (r-increment (* (cl-first increments) times))
         (g-increment (* (cl-second increments) times))
         (b-increment (* (cl-third increments) times))
         (op (if (winddown--light-p) #'- #'+))
         (new-r (funcall op r r-increment))
         (new-g (funcall op g g-increment))
         (new-b (funcall op b b-increment)))
    (plist-put face-info :rgb (list new-r new-g new-b))
    (if (winddown--light-p)
        (color-rgb-to-hex (max 0 new-r) (max 0 new-g) (max 0 new-b))
      (color-rgb-to-hex (min 1.0 new-r) (min 1.0 new-g) (min 1.0 new-b)))))

(defun winddown--color-increse (foreground)
  (let* ((rgb (color-name-to-rgb foreground))
         (r (cl-first rgb))
         (g (cl-second rgb))
         (b (cl-second rgb))
         (delta (/ (* 60 winddown-work-minutes) winddown--update-interval 1.0)))
    (if (winddown--light-p)
        (list :foreground foreground
              :increments (list (/ r delta) (/ g delta) (/ r delta))
              :rgb (list r g b))
      (list :foreground foreground
            :increments (list (/ (- 1.0 r) delta) (/ (- 1.0 g) delta) (/ (- 1.0 b) delta))
            :rgb (list r g b)))))

(defun winddown--update ()
  (cl-incf winddown--update-times)
  (if (> (* winddown--update-times winddown--update-interval) (* winddown-work-minutes 60))
      (winddown-clear)
    (cl-loop for face in (face-list)
             for foreground = (face-foreground face)
             when (and foreground (not (eq face 'winddown--dummy)))
             do
             (let ((face-info (gethash face winddown--face-info)))
               (if face-info
                   (set-face-foreground face (winddown--update-color face-info 1))
                 (let ((fi (winddown--color-increse foreground)))
                   (puthash face fi winddown--face-info)
                   (set-face-foreground face (winddown--update-color fi winddown--update-times))))))))

(defun winddown--save-face-info ()
  (cl-loop for face in (face-list)
           when (face-foreground face)
           do
           (puthash face (winddown--color-increse it) winddown--face-info)))

(defun winddown--restore-faces ()
  (cl-loop for face being each hash-keys of winddown--face-info
           using (hash-values face-info)
           do
           (set-face-foreground face (plist-get face-info :foreground)))
  (clrhash winddown--face-info))

;;;###autoload
(defun winddown ()
  (interactive)
  (winddown--save-face-info)
  (setq winddown--timer
        (run-with-timer winddown--update-interval winddown--update-interval #'winddown--update)
        winddown--update-times 0))

;;;###autoload
(defun winddown-clear ()
  (interactive)
  (winddown--restore-faces)
  (cancel-timer winddown--timer))

(provide 'winddown)

;;; winddown.el ends here
