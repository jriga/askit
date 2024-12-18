;;; ask-org.el --- ask org integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Homepage:   https://github.com/jriga/askit

;;; Commentary:

;; Org simple integration with ask

;;; Code:

(require 'ask-core)
(require 'tempo)
(require 'org-tempo)

;; `<p` expands to #+begin_prompt \n \n #+end_prompt
(setq org-structure-template-alist
      (append org-structure-template-alist '(("p" . "prompt"))))

;; `<pr` expands to #+properties: add ask-properties for org mode
(setq org-tempo-keywords-alist
      (append org-tempo-keywords-alist '(("pr" . "properties"))))


(defvar ask-clients-reg (make-hash-table :test 'equal))
(defvar ask-org-properties-token "#+properties:")


(defun ask-org-project-properties ()
  "Extract the properties of the current org file."
  (save-excursion
    (goto-char (point-min))
    (search-forward ask-org-properties-token nil t)
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
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
          (insert (concat " " token)))))))

(defun ask-org-client-add (options)
  "Add a newly created client with OPTIONS to the client registry."
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


(defun ask-org-parse-clean-string (input)
  "Strip unnecessary tokens from INPUT string."
  (string-trim (string-replace "[[]]" "" input)))

(defun ask-org-parse-raw-prompt (input)
  "Parse a raw input string, extracting attachments and cleaning the prompt.
INPUT is the raw input string containing potential file links."
  (let* ((attachments '())
         (cleaned-prompt
          (replace-regexp-in-string
           "\\[\\[\\([^]]+\\)\\]\\]"
           (lambda (match)
             (let ((file-path (match-string 1 match)))
               (push (string-replace "file:" "" file-path) attachments)
               ""))
           input nil nil 1)))
    (if attachments
       (list
        :prompt (ask-org-parse-clean-string cleaned-prompt)
        :attachments (nreverse attachments))
       input)))


(defun ask-prompt-at-point ()
  "Send prompt at point."
  (interactive)
  (save-excursion
    (let ((request-options  (when (re-search-backward "^#\\+begin_prompt" nil t)
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
                           (append '() `(,(ask-org-parse-raw-prompt (ask-prompt-with-org-format prompt))) request-options))))
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
