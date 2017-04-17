;;; gnutls-tests.el --- Test suite for gnutls.el

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Run this with `GNUTLS_TEST_VERBOSE=1' to get verbose debugging.

;;; Code:

(require 'ert)
(require 'cl)
(require 'gnutls)
(require 'hex-util)

(defvar gnutls-tests-message-prefix "")

(defsubst gnutls-tests-message (format-string &rest args)
  (when (getenv "GNUTLS_TEST_VERBOSE")
    (apply #'message (concat "gnutls-tests: " gnutls-tests-message-prefix format-string) args)))

;; Minor convenience to see strings more easily (without binary data).
(defsubst gnutls-tests-hexstring-equal (a b)
  (and (stringp a) (stringp b) (string-equal (encode-hex-string a) (encode-hex-string b))))

(defvar gnutls-tests-tested-macs
  (remove-duplicates
   (append '(MD5 SHA1 SHA224 SHA256 SHA384 SHA512)
           (mapcar 'car (gnutls-macs)))))

(defvar gnutls-tests-tested-digests
  (remove-duplicates
   (append '(MD5 SHA1 SHA224 SHA256 SHA384 SHA512)
           (mapcar 'car (gnutls-digests)))))

(defvar gnutls-tests-tested-ciphers
  (remove-duplicates
   ; these cause FPEs or SEGVs
   (remove-if (lambda (e) (memq e '(ARCFOUR-128)))
              (mapcar 'car (gnutls-ciphers)))))

(defvar gnutls-tests-mondo-strings
  (list
   ""
   "some data"
   "lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data "
   "data and more data to go over the block limit!"
   "data and more data to go over the block limit"
   (format "some random data %d%d" (random) (random))))

(ert-deftest test-gnutls-000-availability ()
  "Test the GnuTLS hashes and ciphers availability."
  (skip-unless (gnutls-available-p))
  (setq gnutls-tests-message-prefix "availability: ")
  (let ((macs (gnutls-macs))
        (digests (gnutls-digests))
        (ciphers (gnutls-ciphers)))
    (dolist (mac gnutls-tests-tested-macs)
      (let ((plist (cdr (assq mac macs))))
        (gnutls-tests-message "MAC %s %S" mac plist)
        (dolist (prop '(:mac-algorithm-id :mac-algorithm-length :mac-algorithm-keysize :mac-algorithm-noncesize))
          (should (plist-get plist prop)))
        (should (eq 'gnutls-mac-algorithm (plist-get plist :type)))))
    (dolist (digest gnutls-tests-tested-digests)
      (let ((plist (cdr (assq digest digests))))
        (gnutls-tests-message "digest %s %S" digest plist)
        (dolist (prop '(:digest-algorithm-id :digest-algorithm-length))
          (should (plist-get plist prop)))
        (should (eq 'gnutls-digest-algorithm (plist-get plist :type)))))
    (dolist (cipher gnutls-tests-tested-ciphers)
      (let ((plist (cdr (assq cipher ciphers))))
        (gnutls-tests-message "cipher %s %S" cipher plist)
        (dolist (prop '(:cipher-id :cipher-blocksize :cipher-keysize :cipher-ivsize))
          (should (plist-get plist prop)))
        (should (eq 'gnutls-symmetric-cipher (plist-get plist :type)))))))

(ert-deftest test-gnutls-001-hashes-digests ()
  "Test the GnuTLS hash digests against the built-in `secure-hash'."
  (skip-unless (gnutls-available-p))
  (setq gnutls-tests-message-prefix "digest internal verification: ")
  (let ((macs (gnutls-macs)))
    ;; These are the digest algorithms currently supported by
    ;; `secure-hash'. Unfortunately this list can't be obtained
    ;; programmatically.
    (dolist (sym '(md5 sha1 sha224 sha256 sha384 sha512))
      (let* ((mac (intern (upcase (symbol-name sym))))
             (plist (cdr (assq mac macs))))
        (gnutls-tests-message "Checking digest MAC %s %S" mac plist)
        (dolist (input gnutls-tests-mondo-strings)
          (should (gnutls-tests-hexstring-equal
                   (gnutls-hash-digest mac input)
                   (secure-hash sym input nil nil t))))))))

(ert-deftest test-gnutls-002-hashes-digests ()
  "Test some GnuTLS hash digests against pre-defined outputs."
  (skip-unless (gnutls-available-p))
  (setq gnutls-tests-message-prefix "digest external verification: ")
  (let ((macs (gnutls-macs)))
    (dolist (test '(("57edf4a22be3c955ac49da2e2107b67a" "12345678901234567890123456789012345678901234567890123456789012345678901234567890" MD5)
                    ("d174ab98d277d9f5a5611c2c9f419d9f" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" MD5)
                    ("c3fcd3d76192e4007dfb496cca67e13b" "abcdefghijklmnopqrstuvwxyz" MD5)
                    ("f96b697d7cb7938d525a2f31aaf161d0" "message digest" MD5)
                    ("900150983cd24fb0d6963f7d28e17f72" "abc" MD5)
                    ("0cc175b9c0f1b6a831c399e269772661" "a" MD5)
                    ("a9993e364706816aba3e25717850c26c9cd0d89d" "abc" SHA1)
                    ("a9993e364706816aba3e25717850c26c9cd0d89d" "abc" "SHA1"))) ; check string ID for digest
      (destructuring-bind (hash input mac) test
        (let ((plist (cdr (assq mac macs)))
              result)
        (gnutls-tests-message "%s %S" mac plist)
        (setq result (encode-hex-string (gnutls-hash-digest mac input)))
        (gnutls-tests-message "%S => result %S" test result)
        (should (string-equal result hash)))))))

(ert-deftest test-gnutls-003-hashes-hmacs ()
  "Test some predefined GnuTLS HMAC outputs for SHA256."
  (skip-unless (gnutls-available-p))
  (setq gnutls-tests-message-prefix "HMAC verification: ")
  (let ((macs (gnutls-macs)))
    (dolist (test '(("f5c5021e60d9686fef3bb0414275fe4163bece61d9a95fec7a273746a437b986" "hello\n" "test" SHA256)
                    ("46b75292b81002fd873e89c532a1b8545d6efc9822ee938feba6de2723161a67" "more and more data goes into a file to exceed the buffer size" "test" SHA256)
                    ("81568ba71fa2c5f33cc84bf362466988f98eba3735479100b4e8908acad87ac4" "more and more data goes into a file to exceed the buffer size" "very long key goes here to exceed the key size" SHA256)
                    ("4bc830005783a73b8112f4bd5f4aa5f92e05b51e9b55c0cd6f9a7bee48371def" "more and more data goes into a file to exceed the buffer size" "" "SHA256") ; check string ID for HMAC
                    ("4bc830005783a73b8112f4bd5f4aa5f92e05b51e9b55c0cd6f9a7bee48371def" "more and more data goes into a file to exceed the buffer size" "" SHA256)))
      (destructuring-bind (hash input key mac) test
        (let ((plist (cdr (assq mac macs)))
              result)
          (gnutls-tests-message "%s %S" mac plist)
          (setq result (encode-hex-string (gnutls-hash-mac mac (copy-sequence key) input)))
          (gnutls-tests-message "%S => result %S" test result)
          (should (string-equal result hash)))))))


(defun gnutls-tests-pad-or-trim (s exact)
  "Pad or trim string S to EXACT numeric size."
  (let ((e (number-to-string exact)))
    (format (concat "%" e "." e "s") s)))

(defun gnutls-tests-pad-to-multiple (s blocksize)
  "Pad string S to BLOCKSIZE numeric size."
  (let* ((e (if (string= s "")
               blocksize
              (* blocksize (ceiling (length s) blocksize))))
         (out (concat s (make-string (- e (length s)) ? ))))
    ;; (gnutls-tests-message "padding %S to length %d for blocksize %d: => %S" s e blocksize out)
    out))

;; ;;; Testing from the command line:
;; ;;; echo e36a9d13c15a6df23a59a6337d6132b8f7cd5283cb4784b81141b52343a18e5f5e5ee8f5553c23167409dd222478bc30 | perl -lne 'print pack "H*", $_' | openssl enc -aes-128-ctr -d  -nosalt -K 6d796b657932 -iv 696e697432 | od -x
(ert-deftest test-gnutls-004-symmetric-ciphers ()
  "Test the GnuTLS symmetric ciphers"
  (skip-unless (gnutls-available-p))
  (setq gnutls-tests-message-prefix "symmetric cipher verification: ")
  ;; we expect at least 10 ciphers
  (should (> (length (gnutls-ciphers)) 10))
  (let ((keys '("mykey" "mykey2"))
        (inputs gnutls-tests-mondo-strings)
        (ivs '("" "-abc123-" "init" "ini2"))
        (ciphers (remove-if
                  (lambda (c) (plist-get (cdr (assq c (gnutls-ciphers)))
                                    :cipher-aead-capable))
                  gnutls-tests-tested-ciphers)))

    (dolist (cipher ciphers)
      (dolist (iv ivs)
        (dolist (input inputs)
          (dolist (key keys)
            (gnutls-tests-message "%S, starting key %S IV %S input %S" (assq cipher (gnutls-ciphers)) key iv input)
            (let* ((cplist (cdr (assq cipher (gnutls-ciphers))))
                   (key (gnutls-tests-pad-or-trim key (plist-get cplist :cipher-keysize)))
                   (input (gnutls-tests-pad-to-multiple input (plist-get cplist :cipher-blocksize)))
                   (iv (gnutls-tests-pad-or-trim iv (plist-get cplist :cipher-ivsize)))
                   (data (gnutls-symmetric-encrypt cplist (copy-sequence key) (copy-sequence iv) input))
                   (reverse (gnutls-symmetric-decrypt cplist (copy-sequence key) (copy-sequence iv) data)))
              (gnutls-tests-message "%s %S" cipher cplist)
              (gnutls-tests-message "key %S IV %S input %S => hexdata %S and reverse %S" key iv input (encode-hex-string data) reverse)
              (should-not (gnutls-tests-hexstring-equal input data))
              (should-not (gnutls-tests-hexstring-equal data reverse))
              (should (gnutls-tests-hexstring-equal input reverse)))))))))

(ert-deftest test-gnutls-005-aead-ciphers ()
  "Test the GnuTLS AEAD ciphers"
  (skip-unless (gnutls-available-p))
  (setq gnutls-tests-message-prefix "AEAD verification: ")
  (let ((keys '("mykey" "mykey2"))
        (inputs gnutls-tests-mondo-strings)
        (ivs '("" "-abc123-" "init" "ini2"))
        (auths '(nil
                 ""
                 "auth data"
                 "auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data "
                 "AUTH data and more data to go over the block limit!"
                 "AUTH data and more data to go over the block limit"))
        (ciphers (remove-if
                  (lambda (c) (or (null (plist-get (cdr (assq c (gnutls-ciphers)))
                                              :cipher-aead-capable))))
                  gnutls-tests-tested-ciphers)))

    (dolist (cipher ciphers)
      (dolist (iv ivs)
        (dolist (input inputs)
          (dolist (auth auths)
            (dolist (key keys)
              (gnutls-tests-message "%S, starting key %S IV %S input %S auth %S" (assq cipher (gnutls-ciphers)) key iv input auth)
              (let* ((cplist (cdr (assq cipher (gnutls-ciphers))))
                     (key (gnutls-tests-pad-or-trim key (plist-get cplist :cipher-keysize)))
                     (input (gnutls-tests-pad-to-multiple input (plist-get cplist :cipher-blocksize)))
                     (iv (gnutls-tests-pad-or-trim iv (plist-get cplist :cipher-ivsize)))
                     (data (gnutls-symmetric-encrypt cplist (copy-sequence key) (copy-sequence iv) input (copy-sequence auth)))
                     (reverse (gnutls-symmetric-decrypt cplist (copy-sequence key) (copy-sequence iv) data (copy-sequence auth))))
                (gnutls-tests-message "%s %S" cipher cplist)
                (gnutls-tests-message "key %S IV %S input %S auth %S => hexdata %S and reverse %S" key iv input auth (encode-hex-string data) reverse)
                (should-not (gnutls-tests-hexstring-equal input data))
                (should-not (gnutls-tests-hexstring-equal data reverse))
                (should (gnutls-tests-hexstring-equal input reverse))))))))))

;; (ert-deftest test-nettle-006-pbkdf2-RFC-6070 ()
;;     "Test the GnuTLS PBKDF2 SHA1 hashing with the RFC 6070 test set"
;;     (should (string-equal (encode-hex-string (nettle-pbkdf2 "pass\000word" "sa\000lt" 4096 16 "sha1"))
;;                           "56fa6aa75548099dcc37d7f03425e0c3"))
;;     (let ((tests '("0c60c80f961f0e71f3a9b524af6012062fe037a6:password:salt:1:x:sha1"
;;                    "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957:password:salt:2:x:sha1"
;;                    "4b007901b765489abead49d926f721d065a429c1:password:salt:4096:x:sha1"
;;                    ;; "eefe3d61cd4da4e4e9945b3d6ba2158c2634e984:password:salt:16777216:x:sha1" ;; enable for a speed test :)
;;                    "3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038:passwordPASSWORDpassword:saltSALTsaltSALTsaltSALTsaltSALTsalt:4096:x:sha1"))
;;           test expected)
;;       (while (and tests (setq test (split-string (pop tests) ":")))
;;         (setq expected (pop test))
;;         (setf (nth 2 test) (string-to-number (nth 2 test)))
;;         (setf (nth 3 test) (length (decode-hex-string expected)))
;;         ;; (message "Testing 006-pbkdf2-RFC-6070 %S" test)
;;         (should (string-equal (encode-hex-string (apply 'nettle-pbkdf2 test))
;;                               expected)))))

;; (ert-deftest test-nettle-007-rsa-verify ()
;;     "Test the GnuTLS RSA signature verification"
;;     ;; signature too short
;;     (should-error (nettle-rsa-verify "Test the GnuTLS RSA signature"
;;                                      ""
;;                                      "Test the GnuTLS RSA signature"
;;                                      "sha1"))

;;     ;; key too short
;;     (should-error (nettle-rsa-verify "Test the GnuTLS RSA signature"
;;                                      "Test the GnuTLS RSA signature"
;;                                      ""
;;                                      "sha1"))

;;     ;; invalid hashing method
;;     (should-error (nettle-rsa-verify "Test the GnuTLS RSA signature"
;;                                      "Test the GnuTLS RSA signature"
;;                                      ""
;;                                      "no such method"))

;;     ;; key generated with:
;;     ;; openssl genrsa -out privkey.pem 2048
;;     ;; openssl rsa -in privkey.pem -pubout > pubkey.pem
;;     (let* ((key (substring "
;; -----BEGIN PUBLIC KEY-----
;; MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAreGA/Qky9W3izQV0kzba
;; 7wKl/wzwxkbbQxvcUqUT1krgAbO/n1tYFjXdJZoWwbMO/qv7NRoMDY4yPWGpsQfY
;; +PSIknAhTZVbgwXrm/wb37+hKRKax2UZ9A/Rx4vJZRYlkpvZ9LbBziseFNN7SMWW
;; qkjBO/NeT8/I9mURDa+4RoYfT6ZwjTvt808PH7uIghk+MHAx9EMBAfafF1Jn9TqW
;; y+Hgdqik9sZteMvCumvGK4grSwzdfPO5I05tt/0I7QVPxlXbHIk/bBsE7mpgOxur
;; P0DAkFKtYDM7oZPBwB6X778ba2EEFKPpVIyzw/jlDPd9PB6gE6dixmax3Hlg69RI
;; EwIDAQAB
;; -----END PUBLIC KEY-----
;; " 28 426))
;;            ;; 24 skipped bytes are the header
;;            (key-bitstring (substring (base64-decode-string key) 24)))
;;     ;; invalid signature, valid key
;;     (should-not (nettle-rsa-verify "Test the GnuTLS RSA signature"
;;                                    "Test the GnuTLS RSA signature"
;;                                    key-bitstring
;;                                    "sha1"))
;;     ;; valid signature, valid key
;;     ; doesn't work; generated with "openssl rsautl -sign -in /tmp/test -inkey /tmp/privkey.pem" but contains other baggage
;;     (should (nettle-rsa-verify "Test the GnuTLS RSA signature"
;;                                (decode-hex-string "abf710d920de0a210167e62995d5cb06fb0ff6a3f81e2f1965dd3f4716883ab61b7dec40d1ebde89b0657473a434d0333177f183f71a9f4b84a49781b1e4bc440e042f2eb4441000ba07168cdb190c5aebba8c433420f6fc28b6997cbfee061170210bfa65294199e6d6c8c5e1a16421942371f6115d77263b859a75645b6b70d56f14ad378c8499318ff05eda9d24a61d854a3d7f6b67b037abb8d25e4b11ca3e42bdb823cfac34c70057ecd55cbb8449346c0824b46f6c668d14f1744bad7d05470953981df32fde24d2a1f27e58bf9e7d99b20b39b25844c53945dcbbd8b406e78bc0b8aee48c0ec8a26e70301eeeb12ba733e0baf7b82c8e25ac3ee89291")
;;                                key-bitstring
;;                                "sha1"))
;; ))

;; ;; (message (encode-hex-string (nettle-pbkdf2 "password" "salt" 1 20 "sha1")))

(provide 'gnutls-tests)
;;; gnutls-tests.el ends here
