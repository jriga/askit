;;; askit-prog.el --- Ask coding utilities    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Homepage:   https://github.com/jriga/askit

;;; Commentary:

;; utilities for coding assitant

;;; Code:

(require 'askit-core)

(defun askit-prog-client ()
  "Prompt for the project client information."
  (askit-make-client))

(defun askit-prompt-with-org-format (p)
  "Format prompt P for org mode."
  (concat p "
Only do the following if your response contains some code snippets.
Enclose the code portion of your response with the tag #+begin_src #+end_src for org-mode.
Don't forget to put the code language after the #+begin_src."))

(setq askit-template-prompt "Review the following code, focus on clarity and idiomatic code form
provide code suggestions. Each suggestion have a one line short description.
")

(defun askit-code-review-prompt (code)
  "Prompt for a code review on the given CODE."
  (askit-prompt-with-org-format
   (format "%s\n\nThe tag <code> is used to show the code to review.\n<code>%s</code>"
           askit-template-prompt
           code)))

(defun askit-code-system-prompt (m)
  "Prompt M for the system information."
  (format "You are an expert %s programmer" m))


(defun askit-code-review ()
  "Review code in current buffer."
  (interactive)
  (askit-code-review-region (point-min) (point-max)))

(defun askit-code-review-region (&optional rbe ren)
  "Review code in region between RBE and REN."
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
    (insert (format "\n\n#+begin_prompt\n%s\n#+end_prompt\n#+property: :begin_region %s :end_region %s\n\n" askit-template-prompt rb re))
    ;;; TODO: (insert (org-insert-link link "source file"))
    (insert (askit-prompt (askit-prog-client)
                        (askit-code-review-prompt current-region)
                        :system (askit-code-system-prompt buffer-major-mode)))
    (org-mode)))


(provide 'askit-prog)
;;; askit-prog.el ends here
