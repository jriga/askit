;;; ask.el --- ask claude ai wrapper    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Version:    0.0.1
;; Keywords:   lisp, ai, claude, anthropic
;; Homepage:   https://github.com/jriga/askit
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;; Interacts with claudeAI, support tools
;; no streaming at the moment

;;; Code:

(require 'ask-core)
(require 'ask-tools)
(require 'ask-org)
(require 'ask-blog)
(require 'ask-prog)

(defun ask-history (client)
  (funcall (plist-get client :history)))

(defun ask-usage (client)
  (funcall (plist-get client :usage)))

(defun ask-prompt (client prompt-text &rest options)
  (funcall (plist-get client :prompt) prompt-text options))

(defun ask-save (client name)
  (funcall (plist-get client :save) name))

(defun ask-load (name)
  "Load client"
  (let* ((filename (car (directory-files org-roam-directory t (format ".*%s.*\.org" name))))
         (data (ask-load-saved-file filename)))
    (ask-make-client (read (plist-get data :history))
                     (read (plist-get data :usage))
                     data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ask-deftool ask-get-diff (name &optional remote)
  "Get the diff of a branch name."
  :name   (:string "Name of the target branch")
  :remote (:string "Name of the target remote")
  (shell-command-to-string "git diff"))

(ask-deftool ask-get-commit (message)
  "Commits staged items on branch."
  :message (:string "Commit staged changes")
  (shell-command-to-string (format "git commit -am '%s'" message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ask)
;;; ask.el ends here
(package-buffer-info)
