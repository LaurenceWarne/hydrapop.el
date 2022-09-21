;;; hydrapop.el --- Project-specific popup boards -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/hydrapop.el
;; Package-Requires: ((emacs "27.0") (hydra "0.15.0") (dash "2.19.1") (s "1.13.0"))

;;; Commentary:

;; Project-specific popup boards.  See also https://github.com/abo-abo/hydra/.

;;; Code:

(require 'hydra)
(require 'dash)
(require 's)
(require 'cl-lib)

(defgroup hydrapop
  "Project-specific popup boards."
  :group 'bindings
  :prefix "hydrapop-")

(defcustom hydrapop-underscore-replacement "="
  :group hydrapop)

(cl-defstruct hydrapop-column description entries)
(cl-defstruct hydrapop-entry description key function color)

(defmacro hydrapop-define-board (name body &optional docstring &rest heads)
  (defhydra name body docstring heads))

(defun hydrapop--gen-docstring (banner columns)
  (let* ((s-cols (mapcar (lambda (c) (hydrapop--column-str c (hydrapop--width c)))))
         (split-cols (mapcar #'hydrapop-splitlines s-cols))
         (height (-max (-map #'length split-cols)))
         (banner-processed (hydrapop--v-center-string
                            (s-replace "_" hydrapop-underscore-replacement banner)
                            height)))))

(defun hydrapop--column-str (column width)
  (pcase-let* ((desc (hydrapop-column-description column))
               (heading (hydrapop--center-string desc width))
               (break (make-string width ?-)))
    (s-concat heading "\n" break "\n"
              (mapconcat (lambda (s) (hydrapop--entry-str s width))
                         (hydrapop-column-entries column)
                         "\n"))))

(defun hydrapop--entry-str (entry width)
  (format " __%s__: %s"
          (hydrapop-entry-key entry)
          (hydrapop-entry-description entry)))

(defun hydrapop--center-string (s width)
  (pcase-let ((`(,left ,r) (cl-floor (- width (length s)) 2)))
    (s-concat (make-string (+ r left) ?\ ) s (s-repeat left " "))))

(defun hydrapop--v-center-string (s height)
  "Center the string S vertically to HEIGHT."
  (pcase-let* ((split (s-lines s))
               (`(,left ,r) (cl-floor (- height (length split)) 2))
               (remainder (if (> 0 left) r 0))
               (width (-max (-map #'length split)))
               (padding-line (s-repeat width " ")))
    (s-concat (s-join "\n" (-repeat (+ left remainder) padding-line))
              "\n"
              (s-join "\n" (--map (s-pad-right width " " it) split))
              "\n"
              (s-join "\n" (-repeat left padding-line)))))

(defun hydrapop--center-string (s width)
  (pcase-let ((`(,left ,r) (cl-floor (- width (length s)) 2)))
    (s-concat (s-repeat (+ r left) " ") s (s-repeat left " "))))


(defun hydrapop--width (obj)
  (cond ((hydrapop-entry-p obj) (+ 7 (length (hydrapop-entry-description obj))))
        ((hydrapop-column-p obj) (max (-max (mapcar #'hydrapop--width
                                                    (hydrapop-column-entries obj)))
                                      (length (hydrapop-column-description obj))))
        (t (error "Invalid type %s" (type-of obj)))))


(setq col
      (make-hydrapop-column
       :description "My Cool Stuffs"
       :entries (list (make-hydrapop-entry :description "Open"
                                           :key "O"
                                           :function #'ignore
                                           :color 'blue)
                      (make-hydrapop-entry :description "Close Please"
                                           :key "C"
                                           :function #'ignore
                                           :color 'blue))))

(setq banner "   /\\/\\   ‗‗‗| |‗ ‗‗ ‗| |‗‗‗ 
  /    \\ / ‗ \\ ‗‗/ ‗` | / ‗‗|
 / /\\/\\ \\  ‗‗/ || (‗| | \\‗‗ \\
 \\/    \\/\\‗‗‗|\\‗‗\\‗‗,‗|‗|‗‗‗/")

(defhydra hydra-test (:color blue :hint nil :foreign-keys nil)
  "   /\\/\\   ‗‗‗| |‗ ‗‗ ‗| |‗‗‗ 
  /    \\ / ‗ \\ ‗‗/ ‗` | / ‗‗|
 / /\\/\\ \\  ‗‗/ || (‗| | \\‗‗ \\
 \\/    \\/\\‗‗‗|\\‗‗\\‗‗,‗|‗|‗‗‗/" ("c" ignore))

(provide 'hydrapop)

;;; hydrapop.el ends here
