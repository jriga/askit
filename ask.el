;;; ask.el --- ask claude ai                     -*- lexical-binding: t; -*-
;;; Commentary:

;;

;;; Code:

;;; Options

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
  (shell-command-to-string "git commit -am 'nthn'"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ask)
;;; ask.el  ends here
;; TODO: send images feature
;;       preprocess input to detect org link to images
;;       when present add to request
;; TODO: ask-code-diff-replace
;; TODO: ask-code-diff-replace-region
;; TODO: add tool toggle in client prompt
