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

(defgroup hydrapop nil
  "Project-specific popup boards."
  :group 'bindings
  :prefix "hydrapop-")

(defcustom hydrapop-underscore-replacement "."
  "Underscore replacement in hydra strings."
  :group 'hydrapop
  :type 'string)

(cl-defstruct hydrapop-column description entries)
(cl-defstruct hydrapop-entry description key command color)

;; (defmacro hydrapop-define-board (name body banner columns)
;;   `(let ((docstring (hydrapop--gen-docstring ,banner ,columns)))
;;      (defhydra ,name ,body docstring)))

(defun hydrapop-define-board (name banner columns)
  "Define a popup board with the given NAME, BANNER and COLUMNS."
  (eval (hydrapop-define-board-hydra name banner columns)))

(defun hydrapop-define-board-hydra (name banner columns)
  "Return the generated hydra call from NAME, BANNER and COLUMNS."
  (let ((docstring (hydrapop--gen-docstring banner columns)))
    `(defhydra
       ,name
       (:color pink :columns ,(length columns) :hint nil :exit t)
       ,docstring
       ,@(append
          (-map (lambda (e) (list (hydrapop-entry-key e)
                                  (hydrapop-entry-command e)))
                (apply #'append (-map #'hydrapop-column-entries columns)))
          (list '("q" quit-window "quit" :color blue))))))

(defun hydrapop--gen-docstring (banner columns)
  "Generate a body string for BANNER and COLUMNS."
  (let* ((width (-max (-map #'hydrapop--width columns)))
         (s-cols (--map (hydrapop--column-str it width) columns))
         (split-cols (-map #'s-lines s-cols))
         (height (-max (-map #'length split-cols)))
         (banner-processed (hydrapop--v-center-string
                            (s-replace "_" hydrapop-underscore-replacement banner)
                            height))
         (padding (s-repeat width " "))
         (zipped (apply #'-zip-lists
                        (append (list (s-lines banner-processed))
                                (apply (-partial #'-pad padding) split-cols)))))
    (s-concat "\n" (s-join "\n" (--map (s-join "" it) zipped)) "\n")))

(defun hydrapop--column-str (column width)
  "Return COLUMN stringified and padded to WIDTH."
  (pcase-let* ((desc (hydrapop-column-description column))
               (heading (hydrapop--center-string desc width))
               (break (make-string width ?-)))
    (s-concat heading "\n" break "\n"
              (mapconcat (lambda (s) (hydrapop--entry-str s width))
                         (hydrapop-column-entries column)
                         "\n"))))

(defun hydrapop--entry-str (entry width)
  "Return ENTRY as a string padded to WIDTH."
  (s-pad-right width " " (format " _%s_: %s"
                                 (hydrapop-entry-key entry)
                                 (hydrapop-entry-description entry))))

(defun hydrapop--center-string (s width)
  "Center the string S to WIDTH."
  (pcase-let ((`(,left ,r) (cl-floor (- width (length s)) 2)))
    (s-concat (make-string (+ r left) ?\ ) s (s-repeat left " "))))

(defun hydrapop--v-center-string (s height)
  "Center the string S vertically to HEIGHT."
  (pcase-let* ((split (s-lines s))
               (`(,left ,r) (cl-floor (- height (length split)) 2))
               (remainder (if (< 0 left) r 0))
               (width (+ 3 (-max (-map #'length split))))
               (padding-line (s-repeat width " ")))
    (s-concat (s-join "\n" (-repeat (+ left remainder) padding-line))
              (if (< 0 left) "\n" "")
              (s-join "\n" (--map (s-pad-right width " " it) split))
              (if (< 0 left) "\n" "")
              (s-join "\n" (-repeat left padding-line)))))

(defun hydrapop--width (obj)
  "Return the width in chars of the entry or column OBJ."
  (cond ((hydrapop-entry-p obj) (+ 6 (length (hydrapop-entry-description obj))))
        ((hydrapop-column-p obj) (max (-max (mapcar #'hydrapop--width
                                                    (hydrapop-column-entries obj)))
                                      (length (hydrapop-column-description obj))))
        (t (error "Invalid type %s" (type-of obj)))))


(setq col
      (make-hydrapop-column
       :description "My Cool Stuffs"
       :entries (list (make-hydrapop-entry :description "Open"
                                           :key "O"
                                           :command (lambda() (interactive) (message "hi")))
                      (make-hydrapop-entry :description "Close Please"
                                           :key "C"
                                           :command #'ignore)
                      (make-hydrapop-entry :description "Reopen"
                                           :key "R"
                                           :command #'ignore)
                      (make-hydrapop-entry :description "Deopen"
                                           :key "D"
                                           :command #'ignore)))
      col2
      (make-hydrapop-column
       :description "My Cool Stuffs"
       :entries (list (make-hydrapop-entry :description "Open"
                                           :key "O"
                                           :command (lambda() (interactive) (message "hi")))
                      (make-hydrapop-entry :description "Close Please"
                                           :key "C"
                                           :command #'ignore)
                      (make-hydrapop-entry :description "Reopen"
                                           :key "R"
                                           :command #'ignore))))


(defun hydrapop-github-column ()
  "Return a hydrapop column for Github interaction, requires Magit."
  (require 'magit)
  (cl-flet ((hp-open-gh-url () (interactive)
                            (let* ((remotes (magit-list-remotes))
                                   (remote (if (cl-member "upstream" remotes
                                                          :test #'string=)
                                               "upstream"
                                             (car remotes)))
                                   (url (shell-command-to-string
                                         (format "git remote get-url %s"
                                                 remote))))
                              (browse-url
                               (if (s-starts-with-p "ssh://" url)
                                   (s-concat "http://"
                                             (s-chop-prefix "ssh://git@" url))
                                 url))))
            (hp-pr-current-branch () (interactive) nil))
    (make-hydrapop-column
     :description "Github"
     :entries (list (make-hydrapop-entry :description "Open Repository URL"
                                         :key "o"
                                         :command #'hp-open-gh-url)
                    (make-hydrapop-entry :description "PR Current Branch"
                                         :key "p"
                                         :command #'hydrapop-pr-current-branch)))))


(provide 'hydrapop)

;;; hydrapop.el ends here
