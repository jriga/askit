;;; askit-core.el --- ask core functions    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jerome Riga

;; Author:     Jerome Riga <jriga@zemis.co.uk>
;; Maintainer: Jerome Riga <jriga@zemis.co.uk>
;; Homepage:   https://github.com/jriga/askit

;;; Commentary:

;; Interaction functions for claude AI

;;; Code:
(require 'base64)
(require 'json)

(defgroup askit nil
  "Interact with claude in Emacs."
  :group 'hypermedia)

(defvar askit-api-url    "https://api.anthropic.com/v1/messages")
(defvar askit-models     '("claude-3-opus-20240229" "claude-3-5-sonnet-20241022" "claude-3-5-sonnet-20240620" "claude-3-haiku-20240307"))
(defvar askit-model       (car (last askit-models))) ;; haiku
(defvar askit-version    "2023-06-01")
(defvar askit-max-tokens 1024)
(defcustom askit-api-key #'askit-api-key-retriever
  "Function that returns the api-key as string."
  :type 'function
  :group 'askit)
(defvar askit-temperature 0.0)
(defvar askit-system      "")
(defvar askit-tools       [])
(defvar askit-tool_choice '("type" . "auto"))
(defvar askit-debug       t)
(defconst askit-supported-image-ext '("png" "jpeg" "gif" "webp"))

(defun askit-log (o &optional m)
  (let ((s (or m "-->> %s")))
    (message s o)))

(defun askit-api-key-retriever ()
  "Return api-key string."
  (auth-source-pick-first-password :host "api.anthropic.com" :user "apikey"))


(defun askit-anthropic-api-request
    (messages api-key version model max-tokens system temperature &optional tools tool_choice)
  "Send a request to the Anthropic API with the given API-KEY and MESSAGE."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("x-api-key" . ,(funcall api-key))
            ("anthropic-version" . ,version)
            ("anthropic-beta" . "pdfs-2024-09-25")
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

    (when askit-debug
      (askit-log url-request-extra-headers "Headers: %s")
      (askit-log url-request-data "Data: %s"))

    (let ((buffer (url-retrieve-synchronously askit-api-url)))
      (with-current-buffer buffer
        (goto-char url-http-end-of-headers)
        (let ((json-object-type 'plist))
          (json-read))))))


(defun askit-find-block (response)
  (seq-find (lambda (item)
              (and (plist-get item :type)
                   (string= (plist-get item :type) "text")))
            (plist-get response :content)))


(defun askit-content (response)
  "Extract block content from RESPONSE."
  (let ((block (askit-find-block response)))
    (if block
        (plist-get block :text)
      (aref (plist-get response :content) 0))))

(defun askit-mk-single-msg (content role)
  "Format single message to be sent with CONTENT and ROLE."
  `((("role" . ,role)
     ("content" . ,content))))


(defun askit-mk-multi--prompt (prompt)
  "Return formatted PROMPT for multi message request."
  `(("type" . "text")
    ("text" . ,prompt)))

(defun askit-mk-multi--attachment-encode (file)
  "Encode file content at FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (base64-encode-region (point-min) (point-max) :no-line-break)
    (buffer-string)))

(defun askit-mk-multi--attachment (attachment)
  "Return formatted ATTACHMENT image or document for multi message request."
  (let* ((file-ext   (string-replace "jpg" "jpeg" (file-name-extension attachment)))
         (is-image   (member file-ext askit-supported-image-ext))
         (type       (if is-image "image" "document"))
         (media-type (if is-image (format "image/%s" file-ext) "application/pdf"))
         (data       (askit-mk-multi--attachment-encode attachment)))
    `(("type" . ,type)
      ("source" . (("type" . "base64")
                   ("media_type" . ,media-type)
                   ("data" . ,data))))))

(defun askit-mk-multi-msg (content role)
  "Format multiple messages to be sent with CONTENT and ROLE."
  (let* ((prompt (plist-get content :prompt))
         (attachments (plist-get content :attachments))
         (msg-content (append (mapcar #'askit-mk-multi--attachment attachments)
                              (list (askit-mk-multi--prompt prompt)))))
  `((("role" . ,role)
     ("content" . ,msg-content)))))

(defun askit-mk-msg (content &optional role)
  "Return alist generated from CONTENT with specified ROLE or `user` by default."
  (let ((msg-role (or role "user"))
        (attachments (plist-get content :attachments)))
    (if attachments
        (askit-mk-multi-msg content msg-role)
        (askit-mk-single-msg content msg-role))))

(defun askit-update--history (messages response)
  "Handle prefilling when present"
  (let ((last-msg (car (last messages)))
        (role (plist-get response :role)))
    (append (butlast messages)
            (if (string= (cdar last-msg) role)
                (askit-mk-msg (concat (cdadr last-msg) (askit-content response)) role) ;; combine last msg and response
                (list last-msg (car (askit-mk-msg (plist-get response :content) role)))))))

(defun askit-update--usage (old-usage response)
  (let ((u (plist-get response :usage)))
    (list :in    (+ (plist-get old-usage :in)    (plist-get u :input_tokens))
          :out   (+ (plist-get old-usage :out)   (plist-get u :output_tokens))
          :total (+ (plist-get old-usage :total) (plist-get u :input_tokens)
                    (plist-get u :output_tokens)))))

(defun askit-to-kwd (e)
  (symbol-value (intern (concat ":" (symbol-name e)))))

(defun askit-to-sym (e)
  (symbol-value (intern (concat "askit-" (symbol-name e)))))

(defun askit-to-s (e)
  (substring (symbol-name e) 1))

(defun askit-request-args (m request-options client-options)
  (let* ((call-options (car request-options))
         (prefill (plist-get call-options :prefill))
         (params  (if prefill
                      '(api-key version model max-tokens system temperature)
                    '(api-key version model max-tokens system temperature tools tool_choice)))
         (options (mapcar (lambda (k)
                           (or (plist-get call-options (askit-to-kwd k))
                               (plist-get client-options (askit-to-kwd k))
                               (askit-to-sym k)))
                          params)))
    (append (list (if prefill
                      (append m (askit-mk-msg prefill "assistant"))
                      m))
            options)))

(defun askit-handler-error (r _cb)
  (plist-get r :error))

(defun askit-handler-unsupported (r _cb)
  (askit-log r "Unsupported response >>>---\n%s\n>>>---\nAdd new function"))

(defun askit-handler-message (r cb)
  (if (string= (plist-get r :stop_reason) "tool_use")
      (askit-handler-tool-use r cb)
      (askit-handler-prompt r)))

(defun askit-handler-prompt (r)
  "Return message content."
  (askit-content r))

(defun askit-handler-tool-use (r cb)
  "process claude tool use requset"
  (let* ((tools-list (seq-filter (lambda (e) (string= "tool_use" (plist-get e :type)))
                                 (plist-get r :content)))
         (res-content (mapcar 'askit-call-tool tools-list)))
    (funcall cb res-content)))

(defun askit-call-tool (&rest content)
  "Invoke function defined in tools and requested by claude"
  (let* ((l    (car content))
         (fn   (intern (plist-get l :name)))
         (args (seq-filter 'stringp (plist-get l :input)))
         (_ (askit-log args "AAAAA >>>> %s"))
         (res  (apply fn args)))
    (askit-tool-result res (plist-get l :id))))

(defun askit-tool-result (res id)
  "Return an alist for tool_result"
  `(("type" . "tool_result")
    ("tool_use_id" . ,id)
    ("content" . ,res)))

(defun askit-prefilled-response (msg)
  "Update the client history when prefill has been used."
  (let ((content (cdadar msg)))
    (vector `(:type "text" :text ,content))))

(defmacro askit-get-handler (messages)
  "return a response handler name"
  `(let ((type (or (plist-get response :type) "unsupported"))
         (prefill (plist-get (car options) :prefill)))
    (when (equal type "message")
      (setf usage (askit-update--usage usage response))
      (setf history (askit-update--history ,messages response)))
    (funcall (intern (concat "askit-handler-" type))
             (if prefill
               (plist-put response :content (askit-prefilled-response (last history)))
               response)
             (lambda (re) (askit-send-prompt re)))))

(defun askit-org-roam-save-session (session-name h u m v)
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

(defun askit-make-client (&optional h u &rest client-options)
  "Return a client object with history, usage, and prompt functions."
  (let ((history (or h '()))
        (usage   (or u '(:in 0 :out 0 :total 0))))

    (defun askit-send-prompt (content &rest options)
      (when askit-debug (askit-log content ">>> REQUEST : %s"))
      (setf askit-tools (askit-all-tools-def))
      (let* ((messages (append history (askit-mk-msg content)))
             (params (askit-request-args messages options client-options))
             (response (apply 'askit-anthropic-api-request params)))
        (when askit-debug (askit-log response "RAW RESPONSE : %s"))
        (askit-get-handler (car params))))

    (list
     :history (lambda () history)
     :usage   (lambda () usage)
     :prompt  'askit-send-prompt
     :save    (lambda (name)
                (askit-org-roam-save-session name history usage
                                       (or (plist-get client-options :model) askit-model)
                                       (or (plist-get client-options :version) askit-version))))))


(defun askit-load-saved-file (filename)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper funs

(defun askit-history (client)
  "Return the history of the CLIENT."
  (funcall (plist-get client :history)))

(defun askit-usage (client)
  "Return the usage information of the CLIENT."
  (funcall (plist-get client :usage)))

(defun askit-prompt (client prompt-text &rest options)
  "Prompt the CLIENT with PROMPT-TEXT and OPTIONS."
  (funcall (plist-get client :prompt) prompt-text options))

(defun askit-save (client name)
  "Save the CLIENT with the given NAME."
  (funcall (plist-get client :save) name))

(defun askit-select-model ()
  "Select a model defined in askit-models and insert at point in current buffer."
  (interactive)
  (with-current-buffer
      (insert
       (completing-read "Choose model: " askit-models nil t))))

(defun askit-load (name)
  "Load the client with the given NAME."
  (let* ((filename (car (directory-files org-roam-directory t (format ".*%s.*\\.org" name))))
         (data (askit-load-saved-file filename)))
    (askit-make-client (read (plist-get data :history))
                     (read (plist-get data :usage))
                     data)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; begin tools section

(defvar askit-tools-properties-reg (make-hash-table))

(defmacro askit-deftool (fn-name arg-list desc &rest rest)
  "Macro helper to define tools."
  (let ((properties (butlast rest))
        (body       (car (last rest))))
    (puthash fn-name properties askit-tools-properties-reg)
    `(defun ,fn-name ,arg-list ,desc ,body)))

(defun askit-all-tools-def ()
  "Return all defined tools with deftool."
  (mapcar 'askit-tool-def (hash-table-keys askit-tools-properties-reg)))

(defun askit-tool-def--properties (tool)
  "Return alist of TOOL properties."
  (let ((props (seq-split (gethash tool askit-tools-properties-reg) 2)))
    (mapcar 'askit-tool-def--properties-conv props)))

(defun askit-tool-def--properties-conv (args)
  "Convert ARGS plist to alist."
  (let* ((k (car args))
         (v (cadr args))
         (n (askit-to-s k))
         (type (askit-to-s (car v)))
         (desc (cadr v)))
    `(,n . (("type" . ,type)
            ("description" . ,desc)))))

(defun askit-tool-def--required-args (tool)
  "Return a vector of required arguments for the given TOOL (function symbol)."
  (let* ((func-obj (symbol-function tool))
         (arg-list (help-function-arglist func-obj t))
         (required-args '()))
    (while (and arg-list
                (not (memq (car arg-list) '(&optional &rest))))
      (push (symbol-name (car arg-list)) required-args)
      (setq arg-list (cdr arg-list)))
    (vconcat (nreverse required-args))))

(defun askit-tool-def (tool)
  "Return tool definition for claude ai request.
TOOL, name of a function defined with askit-deftool."
  `(("name" . ,(symbol-name tool))
    ("description" . ,(documentation tool))
    ("input_schema" . (("type" . "object")
                       ("properties" . ,(askit-tool-def--properties tool))
                         ("required" . ,(askit-tool-def--required-args tool))))))

;; end tools section

(provide 'askit-core)
;;; askit-core.el ends here
