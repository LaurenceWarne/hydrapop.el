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

(defcustom hydrapop-ticket-url-f-string nil
  "Format string to be used by `hydrapop-open-ticket'.

Intended for .dir-locals.el usage.

Example value: \"https://my-org.atlassian.net/browse/%s\""
  :group 'hydrapop
  :type 'string)

(defconst hydrapop-key-choices "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defconst hydrapop-db-alist-inhibit
  (list
   (cons
    "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil))))

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
           (car (-difference (-map #'char-to-string (string-to-list hydrapop-key-choices)) used)))
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

;;;###autoload
(defun hydrapop-browse-url (url)
  "Return an interactive function which when called browses URL."
  (lambda () (interactive) (browse-url url)))

;;;###autoload
(defun hydrapop-async-shell-command (cmd &optional no-popup)
  "Return an interactive function which when called will run CMD asynchronously.

If NO-POPUP is non-nil, don't display the shell command buffer popup."
  (lambda () (interactive)
    (let ((display-buffer-alist
           (if no-popup hydrapop-db-alist-inhibit display-buffer-alist)))
      (async-shell-command cmd))))

;;;###autoload
(defun hydrapop-async-shell-command-from-project-root (cmd &optional no-popup)
  "Same as `hydrapop-async-shell-command', but execute CMD from the project root.

If NO-POPUP is non-nil, don't display the shell command buffer popup"
  (lambda () (interactive)
    (let ((default-directory
            (or (and (fboundp #'projectile-project-root) (projectile-project-root))
                (and (require 'project) (project-root (project-current)))))
          (display-buffer-alist
           (if no-popup hydrapop-db-alist-inhibit display-buffer-alist)))
      (async-shell-command cmd))))

;;;###autoload
(defun hydrapop-github-column ()
  "Return a hydrapop column for Github interaction, requires Magit."
  (unless (featurep 'magit)
    (user-error "The column hydrapop-github-column requires magit to be installed"))
  (cl-flet* ((get-remote-url (remote) (--> remote
                                           (shell-command-to-string
                                            (format "git remote get-url %s" it))
                                           (if (s-starts-with-p "ssh://" it)
                                               (s-concat "http://"
                                                         (s-chop-prefix "ssh://git@" it))
                                             it)
                                           (s-trim it)))
             (get-upstream-url () (let* ((remotes (magit-list-remotes))
                                         (remote
                                          (cond ((cl-member "upstream" remotes
                                                            :test #'string=)
                                                 "upstream")
                                                ((cl-member "origin" remotes
                                                            :test #'string=)
                                                 "origin")
                                                (t (car remotes)))))
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

;;;###autoload
(defun hydrapop-open-ticket ()
  "Open the current ticket, determined from the current git branch.

Smart enough to recognise slash prefixes e.g. \"feature/abc-3432-...\".

Requires the variable `hydrapop-ticket-url-f-string' to be set, most likely
in your .dir-locals."
  (interactive)
  (unless (featurep 'magit)
    (user-error "The function `hydrapop-open-ticket' requires magit to be installed"))
  (if-let* ((branch (magit-get-current-branch))
            (match (cadr (s-match (rx bos (? (* anychar) "/")
                                      (group (* anychar) "-" (+ digit))) branch)))
            ;; TODO error if `hydrapop-ticket-url-f-string' is nil here
            (url (format hydrapop-ticket-url-f-string match)))
      (progn
        (message "Opening %s" url)
        (browse-url url))
    (message "Couldn't determine ticket from %s" (magit-get-current-branch))))

(declare-function projectile-install-project "projectile.el")
(declare-function projectile-compile-project "projectile.el")
(declare-function projectile-test-project "projectile.el")

;;;###autoload
(defun hydrapop-projectile-column ()
  "Return a hydrapop column with Projectile commands, requires Projectile."
  (unless (featurep 'projectile)
    (user-error "The column hydrapop-projectile-column requires projectile to be installed"))
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

;;;###autoload
(defun hydrapop-column-from-lists (description &rest list)
  "Make a hydrapop column from LIST with the given DESCRIPTION."
  (make-hydrapop-column
   :description description
   :entries (--map (make-hydrapop-entry :description (cadr it)
                                        :key (car it)
                                        :command (caddr it))
                   list)))

;;; Commands

;;;###autoload
(defun hydrapop-invoke ()
  "Invoke the default hydrapop board."
  (interactive)
  (if hydrapop-board
      (funcall hydrapop-board)
    (message "No board for current project.")))

;;;###autoload
(defun hydrapop-init-dir-locals ()
  "Open an insert a dir-locals template for hydrapop."
  (interactive)
  ;; Based off projectile-edit-dir-locals
  (if (boundp #'projectile-project-root)
      (let ((file (expand-file-name ".dir-locals.el" (projectile-acquire-root))))
        (find-file file)
        (unless (file-exists-p file)
          (unwind-protect
              (insert "((nil . ((eval . (hydrapop-define-board hydrapop-project-board
		   \"Your fancy Ascii art here\"
		   (list (hydrapop-projectile-column) (hydrapop-github-column))))
	 (hydrapop-board . hydrapop-project-board/body))))")
            (save-buffer))))))

(provide 'hydrapop)

;;; hydrapop.el ends here
