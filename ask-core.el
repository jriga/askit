;;; ask-core.el --- ask core functions    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Version:    0.0.1
;; Keywords:   lisp, ai, claude, anthropic
;; Homepage:   https://github.com/jriga/askit
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;; Interaction functions for claude AI

;;; Code:
(require 'base64)

(defgroup ask nil
  "Interact with claude in Emacs."
  :group 'hypermedia)

(defvar ask-api-url    "https://api.anthropic.com/v1/messages")
(defvar ask-models     '("claude-3-opus-20240229" "claude-3-5-sonnet-20241022" "claude-3-5-sonnet-20240620" "claude-3-haiku-20240307"))
(defvar ask-model       (car (last ask-models))) ;; haiku
(defvar ask-version    "2023-06-01")
(defvar ask-max-tokens 1024)
(defcustom ask-api-key #'ask-api-key-from-auth-source
  "Function that returns the api-key as string."
  :type 'function
  :group 'ask)
(defvar ask-temperature 0.0)
(defvar ask-system      "")
(defvar ask-tools       [])
(defvar ask-tool_choice '("type" . "auto"))
(defvar ask-debug       t)
(defconst ask-supported-image-ext '("png" "jpeg" "gif" "webp"))

(defun ask-log (o &optional m)
  (let ((s (or m "-->> %s")))
    (message s o)))

(defun ask-api-key-from-auth-source ()
  "Return api-key string."
  (auth-source-pick-first-password :host "api.anthropic.com" :user "apikey"))

(defun ask-anthropic-api-request
    (messages api-key version model max-tokens system temperature &optional tools tool_choice)
  "Send a request to the Anthropic API with the given API-KEY and MESSAGE."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("x-api-key" . ,(funcall api-key))
            ("anthropic-version" . ,version)
            ("content-type" . "application/json")))
         (request-data
          `(("model" . ,model)
            ("max_tokens" . ,max-tokens)
            ("system" . ,system)
            ("temperature" . ,temperature)
            ("messages" . ,messages)))
         (url-request-data
          (json-encode
           (if (and tools (not (seq-empty-p tools)))
               (append request-data `(("tools" . ,tools)
                                      ("tool_choice" . (,tool_choice))))
             request-data))))

    (when ask-debug
      (ask-log url-request-extra-headers "Headers: %s")
      (ask-log url-request-data "Data: %s"))

    (let ((buffer (url-retrieve-synchronously ask-api-url)))
      (with-current-buffer buffer
        (goto-char url-http-end-of-headers)
        (let ((json-object-type 'plist))
          (json-read))))))


(defun ask-find-block (response)
  (seq-find (lambda (item)
              (and (plist-get item :type)
                   (string= (plist-get item :type) "text")))
            (plist-get response :content)))


(defun ask-content (response)
  "Extract block content from RESPONSE."
  (let ((block (ask-find-block response)))
    (if block
        (plist-get block :text)
      (aref (plist-get response :content) 0))))

(defun ask-mk-single-msg (content role)
  "Format single message to be sent with CONTENT and ROLE."
  `((("role" . ,role)
     ("content" . ,content))))


(defun ask-mk-multi--prompt (prompt)
  "Return formatted PROMPT for multi message request."
  `(("type" . "text")
    ("text" . ,prompt)))

(defun ask-mk-multi--attachment-encode (file)
  "Encode file content at FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (base64-encode-region (point-min) (point-max) :no-line-break)
    (buffer-string)))

(defun ask-mk-multi--attachment (attachment)
  "Return formatted ATTACHMENT image or document for multi message request."
  (let* ((file-ext   (string-replace "jpg" "jpeg" (file-name-extension attachment)))
         (is-image   (member file-ext ask-supported-image-ext))
         (type       (if is-image "image" "document"))
         (media-type (if is-image (format "image/%s" file-ext) "application/pdf"))
         (data       (ask-mk-multi--attachment-encode attachment)))
    `(("type" . ,type)
      ("source" . (("type" . "base64")
                   ("media_type" . ,media-type)
                   ("data" . ,data))))))

(defun ask-mk-multi-msg (content role)
  "Format multiple messages to be sent with CONTENT and ROLE."
  (let* ((prompt (plist-get content :prompt))
         (attachments (plist-get content :attachments))
         (msg-content (append (mapcar #'ask-mk-multi--attachment attachments)
                              (list (ask-mk-multi--prompt prompt)))))
  `((("role" . ,role)
     ("content" . ,msg-content)))))

(defun ask-mk-msg (content &optional role)
  "Return alist generated from CONTENT with specified ROLE or `user` by default."
  (let ((msg-role (or role "user"))
        (attachments (plist-get content :attachments)))
    (if attachments
        (ask-mk-multi-msg content msg-role)
        (ask-mk-single-msg content msg-role))))

(defun ask-update--history (messages response)
  "Handle prefilling when present"
  (let ((last-msg (car (last messages)))
        (role (plist-get response :role)))
    (append (butlast messages)
            (if (string= (cdar last-msg) role)
                (ask-mk-msg (concat (cdadr last-msg) (ask-content response)) role) ;; combine last msg and response
                (list last-msg (car (ask-mk-msg (plist-get response :content) role)))))))

(defun ask-update--usage (old-usage response)
  (let ((u (plist-get response :usage)))
    (list :in    (+ (plist-get old-usage :in)    (plist-get u :input_tokens))
          :out   (+ (plist-get old-usage :out)   (plist-get u :output_tokens))
          :total (+ (plist-get old-usage :total) (plist-get u :input_tokens)
                    (plist-get u :output_tokens)))))

(defun ask-to-kwd (e)
  (symbol-value (intern (concat ":" (symbol-name e)))))

(defun ask-to-sym (e)
  (symbol-value (intern (concat "ask-" (symbol-name e)))))

(defun ask-to-s (e)
  (substring (symbol-name e) 1))

(defun ask-request-args (m request-options client-options)
  (let* ((call-options (car request-options))
         (prefill (plist-get call-options :prefill))
         (params  (if prefill
                      '(api-key version model max-tokens system temperature)
                    '(api-key version model max-tokens system temperature tools tool_choice)))
         (options (mapcar (lambda (k)
                           (or (plist-get call-options (ask-to-kwd k))
                               (plist-get client-options (ask-to-kwd k))
                               (ask-to-sym k)))
                          params))
         )
    (append (list (if prefill
                      (append m (ask-mk-msg prefill "assistant"))
                      m))
            options)))

(defun ask-handler-error (r _cb)
  (plist-get r :error))

(defun ask-handler-unsupported (r _cb)
  (ask-log r "Unsupported response >>>---\n%s\n>>>---\nAdd new function"))

(defun ask-handler-message (r cb)
  (if (string= (plist-get r :stop_reason) "tool_use")
      (ask-handler-tool-use r cb)
      (ask-handler-prompt r)))

(defun ask-handler-prompt (r)
  "Return message content."
  (ask-content r))

(defun ask-handler-tool-use (r cb)
  "process claude tool use requset"
  (let* ((tools-list (seq-filter (lambda (e) (string= "tool_use" (plist-get e :type)))
                                 (plist-get r :content)))
         (res-content (mapcar 'ask-call-tool tools-list)))
    (funcall cb res-content)))

(defun ask-call-tool (&rest content)
  "Invoke function defined in tools and requested by claude"
  (let* ((l    (car content))
         (fn   (intern (plist-get l :name)))
         (args (seq-filter 'stringp (plist-get l :input)))
         (_ (ask-log args "AAAAA >>>> %s"))
         (res  (apply fn args)))
    (ask-tool-result res (plist-get l :id))))

(defun ask-tool-result (res id)
  "Return an alist for tool_result"
  `(("type" . "tool_result")
    ("tool_use_id" . ,id)
    ("content" . ,res)))

(defun ask-prefilled-response (msg)
  "Update the client history when prefill has been used."
  (let ((content (cdadar msg)))
    (vector `(:type "text" :text ,content))))

(defmacro ask-get-handler (messages)
  "return a response handler name"
  `(let ((type (or (plist-get response :type) "unsupported"))
         (prefill (plist-get (car options) :prefill)))
    (when (equal type "message")
      (setf usage (ask-update--usage usage response))
      (setf history (ask-update--history ,messages response)))
    (funcall (intern (concat "ask-handler-" type))
             (if prefill
               (plist-put response :content (ask-prefilled-response (last history)))
               response)
             (lambda (re) (ask-send-prompt re)))))

(defun ask-org-roam-save-session (session-name h u m v)
  "Make an org-roam node with title S and return a link to it.
    We eschew the usual org-capture approach for a fast, non-interactive result."
  (let* ((slug (org-roam-node-slug (org-roam-node-create :title session-name)))
         (filename (format "%s/%d-%s.org"
                           (expand-file-name org-roam-directory)
                           (time-convert (current-time) 'integer)
                           slug))
         (org-id-overriding-file-name filename)
         id)
    (with-temp-buffer
      (insert ":PROPERTIES:\n:ID:        \n:END:\n#+title: " session-name)
      (insert "\n#+model: " m)
      (insert "\n#+version: " v)
      (goto-char 25)
      (setq id (org-id-get-create))

      (goto-char (point-max))
      (insert "\n")
      (insert "* History\n#+begin_example\n")
      (prin1 h (current-buffer))
      (insert "\n#+end_example\n\n* Usage\n#+begin_example\n")
      (prin1 u (current-buffer))
      (insert "\n#+end_example\n\n")

      (write-file filename)
      (org-roam-db-update-file filename)
      (format "[[id:%s][%s]]" id session-name))))

(defun ask-make-client (&optional h u &rest client-options)
  "Return a client object with history, usage, and prompt functions."
  (let ((history (or h '()))
        (usage   (or u '(:in 0 :out 0 :total 0))))

    (defun ask-send-prompt (content &rest options)
      (when ask-debug (ask-log content ">>> REQUEST : %s"))
      (setf ask-tools (ask-all-tools-def))
      (let* ((messages (append history (ask-mk-msg content)))
             (params (ask-request-args messages options client-options))
             (response (apply 'ask-anthropic-api-request params)))
        (when ask-debug (ask-log response "RAW RESPONSE : %s"))
        (ask-get-handler (car params))))

    (list
     :history (lambda () history)
     :usage   (lambda () usage)
     :prompt  'ask-send-prompt
     :save    (lambda (name)
                (ask-org-roam-save-session name history usage
                                       (or (plist-get client-options :model) ask-model)
                                       (or (plist-get client-options :version) ask-version))))))


(defun ask-load-saved-file (filename)
  "Extract specific information such model to use from a given file."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((model (progn
                   (re-search-forward "^#\\+model:\\s-*\\(\\w+\\)" nil t)
                   (match-string 1)))
          (version (progn
                     (goto-char (point-min))
                     (re-search-forward "^#\\+version:\\s-*\\(\\w+\\)" nil t)
                     (match-string 1)))
          (history (progn
                     (goto-char (point-min))
                     (when (re-search-forward "^\\* History" nil t)
                       (when (re-search-forward "^#\\+begin_example" nil t)
                         (let ((start (point)))
                           (when (re-search-forward "^#\\+end_example" nil t)
                             (buffer-substring-no-properties start (match-beginning 0))))))))
          (usage (progn
                   (goto-char (point-min))
                   (when (re-search-forward "^\\* Usage" nil t)
                     (when (re-search-forward "^#\\+begin_example" nil t)
                       (let ((start (point)))
                         (when (re-search-forward "^#\\+end_example" nil t)
                           (buffer-substring-no-properties start (match-beginning 0)))))))))
      (list :model model
            :version version
            :history history
            :usage usage))))

(provide 'ask-core)
;;; ask-core.el ends here
