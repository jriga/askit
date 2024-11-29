;;; ask-prog.el --- ask coding utilities    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Version:    0.0.1
;; Keywords:   lisp, ai, claude, anthropic, tools
;; Homepage:   https://github.com/jriga/askit
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;; utilities for coding assitant

;;; Code:

(require 'ask-core)

(defun ask-prog-client ()
  "Prompt for the project client information."
  (ask-make-client))

(defun ask-prompt-with-org-format (p)
  (concat p "
Only do the following if your response contains some code snippets.
Enclose the code portion of your response with the tag #+begin_src #+end_src for org-mode.
Don't forget to put the code language after the #+begin_src."))

(setq ask-template-prompt "Review the following code, focus on clarity and idiomatic code form
provide code suggestions. Each suggestion have a one line short description.
")

(defun ask-code-review-prompt (code)
  "Prompt for a code review on the given code."
  (ask-prompt-with-org-format
   (format "%s\n\nThe tag <code> is used to show the code to review.\n<code>%s</code>"
           ask-template-prompt
           code)))

(defun ask-code-system-prompt (m)
  "Prompt for the system information."
  (format "You are an expert %s programmer" m))


(defun ask-code-review ()
  (interactive)
  (ask-code-review-region (point-min) (point-max)))

(defun ask-code-review-region (&optional rbe ren)
  "Invoke the prompt function with the current region contents and other information."
  (interactive)
  (let* ((rb (or rbe (region-beginning)))
         (re (or ren (region-end)))
         (current-region (buffer-substring-no-properties rb re))
         (buffer-major-mode major-mode)
         (link (progn (goto-char rb) (org-store-link "source"))))
    (switch-to-buffer-other-window "*AskIT*")
    (when (zerop (buffer-size))
      (insert "#+TITLE: AskIT\n#+PROPERTY: header-args:elisp :results pp\n"))
    (goto-char (point-max))
    (insert (format "\n\n#+begin_prompt\n%s\n#+end_prompt\n#+property: :begin_region %s :end_region %s\n\n" ask-template-prompt rb re))
    ;;; TODO: (insert (org-insert-link link "source file"))
    (insert (ask-prompt (ask-prog-client)
                        (ask-code-review-prompt current-region)
                        :system (ask-code-system-prompt buffer-major-mode)))
    (org-mode)))


(provide 'ask-prog)
;;; ask-prog.el ends here
