;;; hydrapop.el --- Project-specific popup boards -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/hydrapop.el
;; Package-Requires: ((emacs "27.0") (hydra "0.15.0"))

;;; Commentary:

;; Project-specific popup boards.  See also https://github.com/abo-abo/hydra/.

;;; Code:

(require 'hydra)
(require 'cl-lib)

(cl-defstruct hydrapop-column description entries)
(cl-defstruct hydrapop-entry description key function color)

(defmacro hydrapop-define-board (name body &optional docstring &rest heads)
  
  (defhydra name body docstring heads))

(defun hydrapop--gen-docstring (banner columns)
  (mapcar ))

(defun hydrapop--column-str (column width)
  (pcase-let* ((desc (hydrapop-column-description column))
               (heading (hydrapop--center-string desc width))
               (break (make-string width ?-)))
    (concat heading "\n" break "\n"
            (mapconcat (lambda (s) (hydrapop--entry-str s width))
                       (hydrapop-column-entries column)
                       "\n"))))

(defun hydrapop--entry-str (entry width)
  (format " __%s__: %s"
          (hydrapop-entry-key entry)
          (hydrapop-entry-description entry)))

(defun hydrapop--center-string (s width)
  (pcase-let ((`(,left ,r) (cl-floor (- width (length s)) 2)))
    (concat (make-string (+ r left) ?\ ) s (make-string left ?\ ))))

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

(provide 'hydrapop)

;;; hydrapop.el ends here
