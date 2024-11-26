;;; ask-blog.el --- ask claude ai                     -*- lexical-binding: t; -*-
;;; Commentary:

;;

;;; Code:

;;; Options

(require 'ask-core)

(defun ask-project-client ()
  "Prompt for the project client information."
  (ask-make-client))

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

(defun ask-prompt-with-markdown-format (p)
  (concat p "
Only do the following if your response contains some code snippets.
Enclose the code portion of your response with the tag ``` and ``` for markdown-mode.
Don't forget to put the code language after the opening ```"))


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

(provide 'ask-blog)
;;; ask-blog.el ends here
