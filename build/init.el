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

(let ((packages '(project-find))
      p)
  (while packages
    (setq p (concat "build/" (symbol-name (car packages))))
    (if (file-exists-p p)
        (add-to-list 'load-path p)
      (setq p (car packages))
      (unless (package-installed-p p) (package-install p)))

    (setq packages (cdr packages))))


;;; init.el ends here
