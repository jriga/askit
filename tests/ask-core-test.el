;;; -*- lexical-binding: t; -*-

(require 'test-helper)
(require 'ask-core)

(describe "variables"
  (it "anthropic url"
      (expect ask-api-url :to-equal "https://api.anthropic.com/v1/messages"))

  (it "lists models"
      (expect ask-models :to-have-same-items-as '("claude-3-opus-20240229"
                                                  "claude-3-5-sonnet-20241022"
                                                  "claude-3-5-sonnet-20240620"
                                                  "claude-3-haiku-20240307"))))
