;;; hydrapop.el --- Project-specific popup boards -*- lexical-binding: t -*-

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Homepage: https://github.com/LaurenceWarne/hydrapop.el
;; Package-Requires: ((emacs "27.0") (hydra "0.15.0"))

;;; Commentary:

;; Project-specific popup boards

;;; Code:

(require 'hydra)

(defmacro hydrapop-define-board (name body &optional docstring &rest heads)
  (defhydra name body docstring heads))

(provide 'hydrapop)

;;; hydrapop.el ends here
