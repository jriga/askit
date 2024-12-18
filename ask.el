;;; ask.el --- Claude ai wrapper    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Keywords:   lisp, ai, claude, anthropic
;; Version:    0.0.1
;; Homepage:   https://github.com/jriga/askit
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;; Interacts with claudeAI, support tools
;; no streaming at the moment

;;; Code:

(require 'ask-core)
(require 'ask-org)
(require 'ask-blog)
(require 'ask-prog)

(defun ask-history (client)
  "Return the history of the CLIENT."
  (funcall (plist-get client :history)))

(defun ask-usage (client)
  "Return the usage information of the CLIENT."
  (funcall (plist-get client :usage)))

(defun ask-prompt (client prompt-text &rest options)
  "Prompt the CLIENT with PROMPT-TEXT and OPTIONS."
  (funcall (plist-get client :prompt) prompt-text options))

(defun ask-save (client name)
  "Save the CLIENT with the given NAME."
  (funcall (plist-get client :save) name))

(defun ask-select-model ()
  "Select a model defined in ask-models and insert at point in current buffer."
  (interactive)
  (with-current-buffer
      (insert
       (completing-read "Choose model: " ask-models nil t))))

(defun ask-load (name)
  "Load the client with the given NAME."
  (let* ((filename (car (directory-files org-roam-directory t (format ".*%s.*\\.org" name))))
         (data (ask-load-saved-file filename)))
    (ask-make-client (read (plist-get data :history))
                     (read (plist-get data :usage))
                     data)))


(provide 'ask)
;;; ask.el ends here
