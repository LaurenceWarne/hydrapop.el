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
(require 'projectile nil t)
(require 'magit nil t)

;;; Custom variables

(defgroup hydrapop nil
  "Project-specific popup boards."
  :group 'bindings
  :prefix "hydrapop-")

(defcustom hydrapop-board nil
  "Board to be opened by `hydrapop-invoke', intended for .dir-locals.el usage."
  :group 'hydrapop
  :type 'function)

(defconst hydrapop-key-choices "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(cl-defstruct hydrapop-column description entries)
(cl-defstruct hydrapop-entry description key command color)

;;; Functions

(defmacro hydrapop-define-board (name banner columns)
  "Define a popup board with the given NAME, BANNER and COLUMNS."
  (declare (indent defun))
  `(hydrapop--define-board ',name ,banner ,columns))

(defun hydrapop--define-board (name banner columns)
  "Define a popup board with the given NAME, BANNER and COLUMNS."
  ;; See https://github.com/abo-abo/hydra/issues/164
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
          (list '("q" ignore "quit" :color blue))))))

(defun hydrapop--gen-docstring (banner columns)
  "Generate a body string for BANNER and COLUMNS."
  (hydrapop--resolve-clashes (-flatten (-map #'hydrapop-column-entries columns)))
  (let* ((width (-max (-map #'hydrapop--width columns)))
         (s-cols (--map (hydrapop--column-str it width) columns))
         (split-cols (-map #'s-lines s-cols))
         (height (-max (-map #'length split-cols)))
         (banner-processed (hydrapop--v-center-string banner height))
         (padding (s-concat "^ ^" (s-repeat (- width 3) " ")))
         (zipped (apply #'-zip-lists
                        (append (apply (-partial #'-pad padding)
                                       (cons (-map #'hydrapop--format-string
                                                   (s-lines banner-processed))
                                             split-cols))))))
    (s-concat "\n" (s-join "\n" (--map (s-join "" it) zipped)) "\n")))

(defun hydrapop--column-str (column width)
  "Return COLUMN stringified and padded to WIDTH."
  (pcase-let* ((desc (hydrapop-column-description column))
               (heading (hydrapop--center-string desc width))
               (break (s-repeat width "-")))
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
    (s-concat (s-repeat (+ r left) " ") s (s-repeat left " "))))

(defun hydrapop--v-center-string (s height)
  "Center the string S vertically to HEIGHT."
  (pcase-let* ((split (s-lines s))
               (`(,left ,r) (cl-floor (- height (length split)) 2))
               (do-pad-r (< (length split) height))
               (remainder (if do-pad-r r 0))
               (width (+ 3 (-max (-map #'length split))))
               (padding-line (s-repeat width " ")))
    (s-concat (s-join "\n" (-repeat (+ left remainder) padding-line))
              (if do-pad-r "\n" "")
              (s-join "\n" (--map (s-pad-right width " " it) split))
              (if do-pad-r "\n" "")
              (s-join "\n" (-repeat left padding-line)))))

(defun hydrapop--width (obj)
  "Return the width in chars of the entry or column OBJ."
  (cond ((hydrapop-entry-p obj) (+ 6 (length (hydrapop-entry-description obj))))
        ((hydrapop-column-p obj) (max (if-let ((ls (mapcar #'hydrapop--width
                                                           (hydrapop-column-entries obj))))
                                          (-max ls)
                                        0)
                                      (length (hydrapop-column-description obj))))
        (t (error "Invalid type %s" (type-of obj)))))

(defun hydrapop--resolve-clashes (entries)
  "Resolve key clashes in ENTRIES."
  (let (used-keys)
    (--each entries (let* ((requested-key (hydrapop-entry-key it))
                           (key (hydrapop--get-key requested-key used-keys)))
                      (cl-pushnew key used-keys)
                      (setf (hydrapop-entry-key it) key)))))

(defun hydrapop--get-key (requested used)
  "Return REQUESTED if it isn't in USED, else select an appropriate key."
  (let ((-compare-fn #'string=))
    (cond ((and (-contains-p used requested) (s-uppercase-p requested))
           (car (-difference hydrapop-key-choices used)))
          ((-contains-p used requested)
           (hydrapop--get-key (s-upcase requested) used))
          (t requested))))

(defun hydrapop--format-string (s)
  "Return a list expression, which when evaled will return S.

Used for inserting underscores into hydra docstrings."
  (let ((u-count (s-count-matches "_" s)))
    (format "%%s(format \"%s\" %s)"
            (s-replace "\\" "\\\\" (s-replace "_" "%s" s))
            (s-repeat u-count "\"_\" "))))

(declare-function magit-get-current-branch "magit.el")
(declare-function magit-push-current-to-pushremote "magit.el")
(declare-function magit-list-remotes "magit.el")

(defun hydrapop-browse-url (url)
  "Return an interactive function which when called browses URL."
  (lambda () (interactive) (browse-url url)))

(defun hydrapop-github-column ()
  "Return a hydrapop column for Github interaction, requires Magit."
  (unless (featurep 'magit)
    (error "The column hydrapop-github-column requires magit to be installed"))
  (cl-flet* ((get-remote-url (remote) (--> remote
                                           (shell-command-to-string
                                            (format "git remote get-url %s" it))
                                           (if (s-starts-with-p "ssh://" it)
                                               (s-concat "http://"
                                                         (s-chop-prefix "ssh://git@" it))
                                             it)
                                           (s-trim it)))
             (get-upstream-url () (let* ((remotes (magit-list-remotes))
                                         (remote (if (cl-member "upstream" remotes
                                                                :test #'string=)
                                                     "upstream"
                                                   (car remotes))))
                                    (get-remote-url remote)))
             (hp-open-gh-url () (interactive) (browse-url (get-upstream-url)))
             (hp-pr-current-branch
              () (interactive)
              (call-interactively #'magit-push-current-to-pushremote)
              (let* ((url (get-remote-url "origin"))
                     (url-open (s-concat url
                                         "/compare/"
                                         (magit-get-current-branch)
                                         "?expand=1")))
                (message "Browsing '%s'" url-open)
                (browse-url url-open))))
    (make-hydrapop-column
     :description "Github"
     :entries (list (make-hydrapop-entry :description "Open Repository URL"
                                         :key "o"
                                         :command #'hp-open-gh-url)
                    (make-hydrapop-entry :description "PR Current Branch"
                                         :key "p"
                                         :command #'hp-pr-current-branch)))))

(declare-function projectile-install-project "projectile.el")
(declare-function projectile-compile-project "projectile.el")
(declare-function projectile-test-project "projectile.el")

(defun hydrapop-projectile-column ()
  "Return a hydrapop column with Projectile commands, requires Projectile."
  (unless (featurep 'projectile)
    (error "The column hydrapop-projectile-column requires projectile to be installed"))
  (make-hydrapop-column
   :description "Projectile"
   :entries (list (make-hydrapop-entry :description "Run tests"
                                       :key "t"
                                       :command #'projectile-test-project)
                  (make-hydrapop-entry :description "Compile"
                                       :key "c"
                                       :command #'projectile-compile-project)
                  (make-hydrapop-entry :description "Install"
                                       :key "i"
                                       :command #'projectile-install-project))))

(defun hydrapop-column-from-lists (description &rest list)
  "Make a hydrapop column from LIST with the given DESCRIPTION."
  (make-hydrapop-column
   :description description
   :entries (--map (make-hydrapop-entry :description (cadr it)
                                        :key (car it)
                                        :command (caddr it))
                   list)))

;;; Commands

(defun hydrapop-invoke ()
  "Invoke the default hydrapop board."
  (interactive)
  (if hydrapop-board
      (funcall hydrapop-board)
    (message "No board for current project.")))

(provide 'hydrapop)

;;; hydrapop.el ends here
