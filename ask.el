;;; ask.el --- ask claude ai                     -*- lexical-binding: t; -*-
;;; Commentary:

;;

;;; Code:

;;; Options
(defvar ask-api-url    "https://api.anthropic.com/v1/messages")
(defvar ask-models     '("claude-3-opus-20240229" "claude-3-5-sonnet-20240620" "claude-3-haiku-20240307"))
(defvar ask-model       (car (last ask-models))) ;; haiku
(defvar ask-version    "2023-06-01")
(defvar ask-max-tokens 1024)
(defvar ask-api-key     (auth-source-pick-first-password :host "api.anthropic.com" :user "apikey"))
(defvar ask-temperature 0.0)
(defvar ask-system      "")
(defvar ask-tools       [])
(defvar ask-tool_choice '("type" . "auto"))
(defvar ask-debug       t)

(defun ask-log (o &optional m)
  (let ((s (or m "-->> %s")))
    (message s o)))

(defun ask-anthropic-api-request
    (messages api-key version model max-tokens system temperature &optional tools tool_choice)
  "Send a request to the Anthropic API with the given API-KEY and MESSAGE."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("x-api-key" . ,api-key)
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
  (let ((block (ask-find-block response)))
    (if block
        (plist-get block :text)
      (aref (plist-get response :content) 0))))


(defun ask-mk-msg (content &optional role)
  `((("role" . ,(or role "user"))
    ("content" . ,content))))

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

(defmacro ask-get-handler (m)
  "return a response handler name"
  `(let ((type (or (plist-get response :type) "unsupported"))
         (prefill (plist-get (car options) :prefill)))
    (when (equal type "message")
      (setf usage (ask-update--usage usage response))
      (setf history (ask-update--history ,m response)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ask-history (client)
  (funcall (plist-get client :history)))

(defun ask-usage (client)
  (funcall (plist-get client :usage)))

(defun ask-prompt (client prompt-text &rest options)
  (funcall (plist-get client :prompt) prompt-text options))

(defun ask-save (client name)
  (funcall (plist-get client :save) name))

(defun ask-load (name)
  "Load client"
  (let* ((filename (car (directory-files org-roam-directory t (format ".*%s.*\.org" name))))
         (data (ask-extract_ filename)))
    (ask-make-client (read (plist-get data :history))
                     (read (plist-get data :usage))
                     data)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ask-extract_ (filename)
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

;;; tools

(defvar ask-tools-properties-reg (make-hash-table))

(defmacro ask-deftool (fn-name arg-list desc &rest rest)
  "Macro helper to define tools."
  (let ((properties (butlast rest))
        (body       (car (last rest))))
    (puthash fn-name properties ask-tools-properties-reg)
    `(defun ,fn-name ,arg-list ,desc ,body)))

(defun ask-tool-def (tool)
  "Return tool definition for claude ai request."
  `(("name" . ,(symbol-name tool))
    ("description" . ,(documentation tool))
    ("input_schema" . (("type" . "object")
                       ("properties" . ,(ask-tool-def--properties tool))
                         ("required" . ,(ask-tool-def--required-args tool))))))

(defun ask-all-tools-def ()
  "Return all defined tools with deftool."
  (mapcar 'ask-tool-def (hash-table-keys ask-tools-properties-reg)))

(defun ask-tool-def--properties (tool)
  "Return alist of tool properties."
  (let ((props (seq-split (gethash tool ask-tools-properties-reg) 2)))
    (mapcar 'ask-tool-def--properties-conv props)))

(defun ask-tool-def--properties-conv (args)
  "Convert plist to alist."
  (let* ((k (car args))
         (v (cadr args))
         (n (ask-to-s k))
         (type (ask-to-s (car v)))
         (desc (cadr v)))
    `(,n . (("type" . ,type)
            ("description" . ,desc)))))

(defun ask-tool-def--required-args (tool)
  "Return a vector of required arguments for the given TOOL (function symbol)."
  (let* ((func-obj (symbol-function tool))
         (arg-list (help-function-arglist func-obj t))
         (required-args '()))
    (while (and arg-list
                (not (memq (car arg-list) '(&optional &rest))))
      (push (symbol-name (car arg-list)) required-args)
      (setq arg-list (cdr arg-list)))
    (vconcat (nreverse required-args))))


(ask-deftool ask-get-diff (name &optional remote)
  "Get the diff of a branch name."
  :name   (:string "Name of the target branch")
  :remote (:string "Name of the target remote")
  (shell-command-to-string "git diff"))

(ask-deftool ask-get-commit (message)
  "Commits staged items on branch."
  :message (:string "Commit staged changes")
  (shell-command-to-string "git commit -am 'nthn'"))


(defun ask-project-client ()
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
    (insert (ask-prompt (ask-project-client)
                        (ask-code-review-prompt current-region)
                        :system (ask-code-system-prompt buffer-major-mode)))
    (org-mode)))

(defun ask-prompt-at-point ()
  (interactive)
  (save-excursion
    (let ((plist  (when (re-search-backward "^#\\+begin_prompt" nil t)
                    (forward-word)
                    (forward-word)
                    (read (concat "(" (buffer-substring-no-properties (point) (line-end-position)) ")"))))
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

(defun ask-prompt-with-markdown-format (p)
  (concat p "
Only do the following if your response contains some code snippets.
Enclose the code portion of your response with the tag ``` and ``` for markdown-mode.
Don't forget to put the code language after the opening ```"))

(setq ask-template-prompt "Review the following code, focus on clarity and idiomatic code form
provide code suggestions. Each suggestion have a one line short description.
")


(setq ask-template-hugo-post
      "You are an experienced blog writer and programmer.
Write a concise and clear blog post conveying the 3 most interesting aspects of the code sample between the tag <code> and </code>.
Prepend your respones with
+++
title = '<post-title />'
date = 2024-09-13T21:40:30+01:00
draft = true
+++

From your response get the most appropriate title and swap it with tag <post-title />
The blog must have a section called '## What we'll cover' listing the major points covered by the article.
")

(defun ask-blog-prompt (code)
  "Return a prompt to create a blog post"
  (ask-prompt-with-markdown-format
   (format "%s\n\n<code>%s</code>\n\nWe are using Hugo for blogging with the markdorwn file format."
           ask-template-hugo-post
           code)))

(defun ask-make-post-region (&optional rbe ren)
  "Create a blog post from region."
  (interactive)
  (let* ((rb (or rbe (region-beginning)))
         (re (or ren (region-end)))
         (current-region (buffer-substring-no-properties rb re))
         (buffer-major-mode major-mode))
    (switch-to-buffer-other-window "*AskIT-Blog*")
    (when (zerop (buffer-size))
      (insert "#+TITLE: AskIT-Blog\n\n"))
    (goto-char (point-max))
    (insert (format "\n\n``` prompt\n%s\n```\n\n `property: :begin_region %s :end_region %s`\n\n"
                    ask-template-hugo-post
                    rb re))
    ;;; TODO: (insert (org-insert-link link "source file"))
    (insert (ask-prompt (ask-project-client)
                        (ask-blog-prompt current-region)
                        :model (cadr ask-models)
                        :system "You are an excellent and engaging blog writer."))
    (markdown-mode)))


(defun ask-make-post ()
  (interactive)
  (ask-make-post-region (point-min) (point-max)))


(provide 'ask)
;;; ask.el  ends here
;; TODO: send images feature
;;       preprocess input to detect org link to images
;;       when present add to request
;; TODO: ask-code-diff-replace
;; TODO: ask-code-diff-replace-region
;; TODO: add tool toggle in client prompt
