;;; -*- lexical-binding: t; -*-

;;; Code:
(require 'test-helper)
(require 'askit-core)

(describe "variables"
  (it "anthropic url"
      (expect askit-api-url :to-equal "https://api.anthropic.com/v1/messages"))

  (it "lists models"
      (expect askit-models :to-have-same-items-as '("claude-3-opus-20240229"
                                                  "claude-3-5-sonnet-20241022"
                                                  "claude-3-5-sonnet-20240620"
                                                  "claude-3-haiku-20240307"))))

(describe "askit-mk-msg"
  (describe "for simple prompt"
     :var ((content "my prompt")
           (role "robot"))
    (it "askit-mk-msg when role provided"
        (expect (askit-mk-msg content role) :to-have-same-items-as '((("role" . "robot")
                                                                    ("content" . "my prompt")))))
    (it "askit-mk-msg when role missing"
        (let ((expected '((("role" . "user")
                           ("content" . "my prompt")))))
          ;; (message (json-encode expected))
          (expect (askit-mk-msg content) :to-have-same-items-as expected))))

  (describe "for prompt with attachments"
     :var ((content '(:prompt "my prompt" :attachments ("tests/assets/test-img.png")))
           (role "robot"))

     (it "askit-mk-msg when role provided"
         (let ((expected '((("role" . "robot")
                            ("content" . ((("type" . "image")
                                           ("source" . (("type" . "base64")
                                                        ("media_type" . "image/png")
                                                        ("data" . "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAABHNCSVQICAgIfAhkiAAAALpJREFUOI1j/P///38GCgALjHHl7jWGmw9vEaXJz86bgZWFFdWAZTtXMfQsnUiUAQ5GtgzpgUkMwU4BCANgQEdZi0FNThWn5it3rzEcOHeYwc3CGdUFMBDpFspQGlOA04DKqfUMvctuw/lMRLkZDxg1YDAYgJEOdp3Yy/Dxy0ecGg5fOIbdAF0VbQZzbROGg+ePMBw8f4RoFzAi58YbD24xbDy8Fa8GIV4BBgUpeQY1WRUGeUk5VAPIAQA8TzgKb19m0wAAAABJRU5ErkJggg=="))))
                                          (("type" . "text")
                                           ("text" . "my prompt"))))))))
           ;; (message (json-encode expected))
           (expect (askit-mk-msg content role) :to-have-same-items-as expected)))))

;;; ends here
