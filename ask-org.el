;;; ask-org.el --- ask claude ai                     -*- lexical-binding: t; -*-
;;; Commentary:

;;

;;; Code:

;;; Options
(require 'ask-core)

(defvar ask-clients-reg (make-hash-table))

(defun ask-org-project-properties ()
  "Extract the properties of the current org file."
  (save-excursion
    (goto-char (point-min))
    (search-forward "#+properties:" nil t)
    (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position) ))
           (args (string-replace "#+properties: " "" line)))
      (mapcar (lambda (x) (if (string-match ":" x) (intern x) x))
              (split-string args " ")))))

(defun ask-rand-session-name ()
  "Return a random string."
  (format "%s" (time-convert (current-time) 'integer)))

(defun ask-add-session-property (session)
  (save-excursion
    (goto-char (point-min))
    (search-forward "#+properties:" nil t)
    (end-of-line)
    (insert (format " :session %s" session))))

(defun ask-org-client-add (options)
  ""
  (let ((session (or (plist-get options :session)
                     (ask-rand-session-name)))
        (client (apply 'ask-make-client `(nil nil options))))
    (puthash session client ask-clients-reg)
    (ask-add-session-property session)
    client))

(defun ask-org-client ()
  "Prompt for the project client information."
  (let* ((options (ask-org-project-properties))
         (session (plist-get options :session))
         (client (gethash session ask-clients-reg nil)))
    (if client client (ask-org-client-add options))))


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
            (insert (apply 'ask-prompt (ask-org-client)
                           (append '() `(,(ask-prompt-with-org-format prompt)) plist))))
        (message "No valid prompt found at point.")))))


(provide 'ask-org)
;;; ask-org.el ends here
