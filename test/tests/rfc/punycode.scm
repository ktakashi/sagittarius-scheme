;; -*- mode:scheme; coding:utf-8; -*-
(import (rnrs)
        (rfc punycode)
        (srfi :64 testing))

(test-begin "RFC Punycode tests")

(test-equal "encode: ASCII input" "abc-123-"
            (punycode-encode "abc-123"))
(test-equal "decode: ASCII input" "abc-123"
            (punycode-decode "abc-123-"))

(test-equal "encode: buecher"
            "bcher-kva"
            (punycode-encode "bücher"))
(test-equal "decode: buecher"
            "bücher"
            (punycode-decode "bcher-kva"))

(test-equal "encode: japanese example"
            "r8jz45g"
            (punycode-encode "例え"))
(test-equal "decode: japanese example"
            "例え"
            (punycode-decode "r8jz45g"))

(test-equal "encode: mixed symbols"
            "--dqo34k"
            (punycode-encode "☃-⌘"))
(test-equal "decode: mixed symbols"
            "☃-⌘"
            (punycode-decode "--dqo34k"))

(test-assert "label?: valid A-label"
             (punycode-label? "xn--bcher-kva"))
(test-assert "label?: missing prefix is not A-label"
             (not (punycode-label? "bcher-kva")))
(test-assert "label?: short payload is invalid"
             (not (punycode-label? "xn--a")))
(test-assert "label?: malformed payload is invalid"
             (not (punycode-label? "xn--ab*")))

(test-assert "label?: disallowed exception U+302E"
             (not (punycode-label? "xn--07jt112bpxg")))
(test-assert "label?: disallowed right-to-left exception"
             (not (punycode-label? "xn--chb89f")))
(test-assert "label?: invalid middle dot context"
             (not (punycode-label? "xn--al-0ea")))
(test-assert "label?: invalid zwj context"
             (not (punycode-label? "xn--11b2er09f")))
(test-assert "label?: valid zwj context"
             (punycode-label? "xn--11b2ezcw70k"))

(test-error "decode: malformed payload raises assertion"
            assertion-violation?
            (punycode-decode "ab*"))

(test-end)
