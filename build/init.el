;;; init.el --- Init file  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Setup for testing.
;;
;;; Code:

(require 'package)

(setq package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(if (file-exists-p "build/project-find")
    (add-to-list 'load-path "build/project-find")
  (error "TODO: use-package project-find"))


;;; init.el ends here
