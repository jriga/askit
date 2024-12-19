;;; askit-org-test --- test askit-org -*- lexical-binding: t; -*-

;;; Code:
(require 'test-helper)
(require 'askit-org)

(describe "askit-org-parse-raw-prompt"
  (it "when input has org links returns a plist"
      (let ((raw-prompt "some text [[/home/jriga/devel/askit/tests/assets/test-img.png]] and this [[file:assets/test-img.png]] finally thin")
            (expected '(:prompt "some text  and this  finally thin"
                        :attachments ("/home/jriga/devel/askit/tests/assets/test-img.png"
                                      "assets/test-img.png"))))
        (expect (askit-org-parse-raw-prompt raw-prompt) :to-have-same-items-as expected)))

  (it "when input has no org links returns the same input"
      (let ((raw-prompt "some text"))
        (expect (askit-org-parse-raw-prompt raw-prompt) :to-equal raw-prompt))))

;;; askit-org-test.el ends here
