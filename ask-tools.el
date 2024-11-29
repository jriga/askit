;;; ask-tools.el --- ask claude ai tools support    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Version:    0.0.1
;; Keywords:   lisp, ai, claude, anthropic, tools
;; Homepage:   https://github.com/jriga/askit
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;; provides tools support for ask package

;;; Code:
(defvar ask-tools-properties-reg (make-hash-table))

(defmacro ask-deftool (fn-name arg-list desc &rest rest)
  "Macro helper to define tools."
  (let ((properties (butlast rest))
        (body       (car (last rest))))
    (puthash fn-name properties ask-tools-properties-reg)
    `(defun ,fn-name ,arg-list ,desc ,body)))

(defun ask-all-tools-def ()
  "Return all defined tools with deftool."
  (mapcar 'ask-tool-def (hash-table-keys ask-tools-properties-reg)))

(defun ask-tool-def--properties (tool)
  "Return alist of TOOL properties."
  (let ((props (seq-split (gethash tool ask-tools-properties-reg) 2)))
    (mapcar 'ask-tool-def--properties-conv props)))

(defun ask-tool-def--properties-conv (args)
  "Convert ARGS plist to alist."
  (let* ((k (car args))
         (v (cadr args))
         (n (ask-to-s k))
         (type (ask-to-s (car v)))
         (desc (cadr v)))
    `(,n . (("type" . ,type)
            ("description" . ,desc)))))

(defun ask-tool-def--required-args (tool)
  "Return a vector of required arguments for the given TOOL (function symbol)."
  (let* ((func-obj (symbol-function tool))
         (arg-list (help-function-arglist func-obj t))
         (required-args '()))
    (while (and arg-list
                (not (memq (car arg-list) '(&optional &rest))))
      (push (symbol-name (car arg-list)) required-args)
      (setq arg-list (cdr arg-list)))
    (vconcat (nreverse required-args))))

(defun ask-tool-def (tool)
  "Return tool definition for claude ai request.
TOOL, name of a function defined with ask-deftool."
  `(("name" . ,(symbol-name tool))
    ("description" . ,(documentation tool))
    ("input_schema" . (("type" . "object")
                       ("properties" . ,(ask-tool-def--properties tool))
                         ("required" . ,(ask-tool-def--required-args tool))))))

(provide 'ask-tools)
;;; ask-tools.el ends here

(package-buffer-info)
