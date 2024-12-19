;;; askit-org.el --- ask org integration    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Homepage:   https://github.com/jriga/askit

;;; Commentary:

;; Org simple integration with ask

;;; Code:

(require 'askit-core)
(require 'tempo)
(require 'org-tempo)

;; `<p` expands to #+begin_prompt \n \n #+end_prompt
(setq org-structure-template-alist
      (append org-structure-template-alist '(("p" . "prompt"))))

;; `<pr` expands to #+properties: add askit-properties for org mode
(setq org-tempo-keywords-alist
      (append org-tempo-keywords-alist '(("pr" . "properties"))))


(defvar askit-clients-reg (make-hash-table :test 'equal))
(defvar askit-org-properties-token "#+properties:")


(defun askit-org-project-properties ()
  "Extract the properties of the current org file."
  (save-excursion
    (goto-char (point-min))
    (search-forward askit-org-properties-token nil t)
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
      (read (concat "(" line ")")))))

(defun askit-rand-session-name ()
  "Return a random string."
  (format "%s" (time-convert (current-time) 'integer)))

(defun askit-add-property (key value)
  "Add a property `KEY` and its value `VALUE` to properties list."
  (save-excursion
    (goto-char (point-min))
    (search-forward askit-org-properties-token nil t)
    (let ((line (thing-at-point 'line))
          (token (format "%s %s" key value)))
      (if (string-match token line)
          nil
        (progn
          (end-of-line)
          (insert (concat " " token)))))))

(defun askit-org-client-add (options)
  "Add a newly created client with OPTIONS to the client registry."
  (let* ((session (or (plist-get options :session)
                     (askit-rand-session-name)))
         (client-options (cons '() (cons '() options)))
         (client (apply 'askit-make-client client-options)))
    (puthash session client askit-clients-reg)
    (askit-add-property :session session)
    client))

(defun askit-org-client ()
  "Prompt for the project client information."
  (let* ((options (askit-org-project-properties))
         (session (plist-get options :session))
         (client (gethash session askit-clients-reg)))
    (if client client (askit-org-client-add options))))


(defun askit-org-parse-clean-string (input)
  "Strip unnecessary tokens from INPUT string."
  (string-trim (string-replace "[[]]" "" input)))

(defun askit-org-parse-raw-prompt (input)
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
        :prompt (askit-org-parse-clean-string cleaned-prompt)
        :attachments (nreverse attachments))
       input)))


(defun askit-prompt-at-point ()
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
            (insert (apply 'askit-prompt (askit-org-client)
                           (append '() `(,(askit-org-parse-raw-prompt (askit-prompt-with-org-format prompt))) request-options))))
        (message "No valid prompt found at point.")))))

(defun askit-dump-client ()
  "Dump the current buffer session."
  (interactive)
  (let* ((options (askit-org-project-properties))
         (client (askit-org-client))
         (link (askit-org-roam-save-session (plist-get options :session)
                             (askit-history client)
                             (askit-usage client)
                             (or (plist-get options :model) askit-model)
                             (or (plist-get options :version) askit-version)))))
  (askit-add-property :link link))

(defun askit-restore-client ()
  "Restore the current buffer session."
  (interactive)
  (let* ((options (askit-org-project-properties))
         (client (askit-load-saved-file (plist-get options :link))))
    (puthash (plist-get options :session) client askit-clients-reg)))

(provide 'askit-org)
;;; askit-org.el ends here
