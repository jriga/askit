;;; ask-org.el --- ask claude ai                     -*- lexical-binding: t; -*-
;;; Commentary:

;;

;;; Code:

;;; Options
(require 'ask-core)

(defun ask-project-client ()
  "Prompt for the project client information."
  (ask-make-client))

(defun ask-prompt-at-point ()
  (interactive)
  (save-excursion
    (let ((plist  (when (re-search-backward "^#\\+begin_prompt" nil t)
                    (forward-word)
                    (forward-word)
                    (read (concat "(" (buffer-substring-no-properties (point) (line-end-position)) ")")))) ;; reads properties
          (prompt (when (re-search-backward "^#\\+begin_prompt" nil t)
                    (forward-line)
                    (buffer-substring-no-properties (point)
                                                    (progn (re-search-forward "^#\\+end_prompt" nil t)
                                                           (forward-line -1)
                                                           (end-of-line)
                                                           (point))))))
      (if prompt
          (progn
            (re-search-forward "^#\\+end_prompt" nil t)
            (insert "\n\n")
            (insert (apply 'ask-prompt (ask-project-client)
                           (append '() `(,(ask-prompt-with-org-format prompt)) plist))))
        (message "No valid prompt found at point.")))))


(provide 'ask-org)
;;; ask-org.el ends here
