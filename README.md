# Thin emacs wrapper for Claude AI

Personal emacs frontend for claude AI this is very much work in progress.

# Usage
  You must define a function named `askit-api-key-retriever` that returns the api key.

```elisp
  (require 'askit)

  (defun askit-api-key-retriever ()
    "Return api-key string."
    (auth-source-pick-first-password :host "api.anthropic.com" :user "apikey"))

```

## Tool use
Declare a tool

```elisp
  (askit-deftool my-web-search (query)
    "Perform a web search with the provided QUERY."
    :query (:string "search query terms")
    (let ((cmd (format "curl -s --compressed \"https://api.search.brave.com/res/v1/web/search?q=%s\"
  -H \"Accept: application/json\"
  -H \"Accept-Encoding: gzip\"
  -H \"X-Subscription-Token: %s\" " query (getenv "BRAVE_API_KEY"))))
      (shell-command-to-string cmd)))
```

# TODOS
- [X] add session support
- [X] dump/restore session
- [X] send attachments
- [X] rename ask to askit
- [ ] change http lib to enable streaming responses
