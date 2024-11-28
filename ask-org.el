;;; ask-org.el --- ask claude ai                     -*- lexical-binding: t; -*-
;;; Commentary:

;;

;;; Code:

;;; Options
(require 'ask-core)

(defvar ask-clients-reg (make-hash-table :test 'equal))
(defvar ask-org-properties-token "#+properties:")

;; (defun ask-org-project-properties ()
;;   "Extract the properties of the current org file."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (search-forward ask-org-properties-token nil t)
;;     (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position) ))
;;            (str-to-replace (format "%s " ask-org-properties-token))
;;            (args (string-replace str-to-replace "" line)))
;;       (mapcar (lambda (x) (if (string-match ":" x) (intern x) x))
;;               (split-string args " ")))))

(defun ask-org-project-properties ()
  "Extract the properties of the current org file."
  (save-excursion
    (goto-char (point-min))
    (search-forward ask-org-properties-token nil t)
    (let* ((line (buffer-substring-no-properties (point) (line-end-position) )))
      (read (concat "(" line ")")))))

(defun ask-rand-session-name ()
  "Return a random string."
  (format "%s" (time-convert (current-time) 'integer)))

(defun ask-add-property (key value)
  "Add a property `KEY` and its value `VALUE` to properties list."
  (save-excursion
    (goto-char (point-min))
    (search-forward ask-org-properties-token nil t)
    (let ((line (thing-at-point 'line))
          (token (format "%s %s" key value)))
      (if (string-match token line)
          nil
        (progn
          (end-of-line)
          (insert token))))))

(defun ask-org-client-add (options)
  ""
  (let* ((session (or (plist-get options :session)
                     (ask-rand-session-name)))
         (client-options (cons '() (cons '() options)))
         (client (apply 'ask-make-client client-options)))
    (puthash session client ask-clients-reg)
    (ask-add-property :session session)
    client))

(defun ask-org-client ()
  "Prompt for the project client information."
  (let* ((options (ask-org-project-properties))
         (session (plist-get options :session))
         (client (gethash session ask-clients-reg)))
    (if client client (ask-org-client-add options))))

(defun ask-prompt-at-point ()
  "Send prompt at point."
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

(defun ask-dump-client ()
  "Dump the current buffer session."
  (interactive)
  (let* ((options (ask-org-project-properties))
         (client (ask-org-client))
         (link (ask-org-roam-save-session (plist-get options :session)
                             (ask-history client)
                             (ask-usage client)
                             (or (plist-get options :model) ask-model)
                             (or (plist-get options :version) ask-version)))))
  (ask-add-property :link link))

(defun ask-restore-client ()
  "Restore the current buffer session."
  (interactive)
  (let* ((options (ask-org-project-properties))
         (client (ask-load-saved-file (plist-get options :link))))
    (puthash (plist-get options :session) client ask-clients-reg)))

(provide 'ask-org)
;;; ask-org.el ends here
