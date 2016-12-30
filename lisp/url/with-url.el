;;; with-url.el --- High-Level URL Interface -*- lexical-binding: t -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: http url

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'puny)
(require 'gnutls)
(require 'mm-url)

(cl-defstruct url-request
  original-url wait timeout read-timeout
  silent inhibit-cookies inhibit-cache headers
  method
  data data-charset data-encoding
  callback redirect-times
  url parsed-url process
  response-size start-time last-read-time timer)

(cl-defmacro with-url ((header-variable
                        url
                        &key wait timeout
                        read-timeout silent
                        inhibit-cookies
                        inhibit-cache
                        headers
                        (method "GET")
                        data
                        (data-charset 'utf-8)
                        data-encoding)
                       &body body)
  "Retrieve URL and execute BODY with point in a buffer with the response.

Example:

  (with-url (headers \"http://fsf.org/\")
    (message \"The size of the FSF front page is %s\" (buffer-size)))

The buffer is killed after BODY has exited.

HEADER-VARIABLE is bound to a structure that contains the response
headers and status.  These can be accessed with `url-header' like this:

  (url-header headers \"Content-Type\")

Case is not significant.

Additional keywords can be given to `with-url' to alter its operation.

:wait t
Normal `with-url' operation is asynchronous.  If this parameter is given,
the retrieval will be synchronous instead.

:timeout SECONDS
Give up after approximately SECONDS seconds and execute BODY.

:read-timeout SECONDS
If no data has been received for the last SECONDS seconds, give
up and execute BODY.

:silent t
Issue no messages during operation.

:inhibit-cookies t
Neither send nor store cookies.

:headers ALIST
Add ALIST to the headers sent over to the server.  This should typically
look like

  ((\"User-Agent\" \"Emacs\"))

If the header name is the same as one of the automatically
generated headers, the value from this list will override the
automatically generated header.  To disable the header
completely, use nil as the value.

Additional elements in this alist are interpreted as the coding
system (defaulting to `utf-8') and the encoding
method (defaulting to `url-encode').

:method GET/POST/etc
The method to use for retrieving an HTTP(S) resource.  This defaults
to GET, and other popular values are POST, UPDATE and PUT.

:data STRING/ALIST
Data to include in the body of the HTTP(S) request when using
POST, UPDATE or PUT.  This can either be a string or an alist of POST values
on this form:

  '((\"NAME\" \"VALUE\")
    (\"submit\")
    ((\"NAME1\" \"VALUE1\")
     (\"NAME2\" \"VALUE2\")))

Elements with several values only make sense with the `multipart'
encoding (see below).

:data-coding-system CODING-SYSTEM
What coding system this data should be encoded as.  This defaults
to `utf-8'.

:data-encoding ENCODING
When using the posting methods, the data is usually encoded in
some fashion.  Supported encodings are `url-form', `multipart'
and `base64'."
  (let ((requestv (cl-gensym "request")))
    `(let ((,requestv 
            (make-url-request :original-url ,url
                              :timeout ,timeout
                              :read-timeout ,read-timeout
                              :silent ,silent
                              :inhibit-cookies ,inhibit-cookies
                              :inhibit-cache ,inhibit-cache
                              :headers ',headers
                              :method ,method
                              :data ,data
                              :data-charset ',data-charset
                              :data-encoding ,data-encoding
                              :start-time (current-time)
                              :last-read-time (current-time)
                              :redirect-times 0)))
       ,(if wait
            `(let ((,header-variable (with-url--fetch ,requestv)))
               ,@body)
          `(progn
             (setf (url-request-callback ,requestv)
                   (lambda ()
                     (let ((,header-variable with-url--headers))
                       ,@body)))
             (with-url--fetch ,requestv))))))

(defun with-url--fetch (req)
  (unless (url-request-url req)
    (setf (url-request-url req) (url-request-original-url req)))
  (setf (url-request-parsed-url req)
        (url-generic-parse-url (url-request-url req)))
  (when (or (url-request-timeout req)
            (url-request-read-timeout req))
    (setf (url-request-timer req)
          (run-at-time 1 1 (lambda ()
                             (with-url--timer req)))))
  (with-current-buffer (generate-new-buffer "*request*")
    (set-buffer-multibyte nil)
    (let* ((coding-system-for-read 'binary)
           (coding-system-for-write 'binary)
           (process
            (make-network-process
             :name (url-request-url req)
             :buffer (current-buffer)
             :host (url-host (url-request-parsed-url req))
             :service (or (url-portspec (url-request-parsed-url req))
                          (if (equal (url-type (url-request-parsed-url req))
                                     "https")
                              443
                            80))
             :nowait t
             :plist (list :request req)
             :tls-parameters
             (and (equal (url-type (url-request-parsed-url req)) "https")
                  (cons 'gnutls-x509pki
                        (gnutls-boot-parameters
                         :hostname (puny-encode-string
                                    (url-host (url-request-parsed-url req))))))
             :sentinel #'with-url--sentinel
             :filter #'with-url--filter)))
      (setf (url-request-process req) process))))

(defun with-url--timer (req)
  (let ((now (float-time)))
    ;; There are two possible timeouts: One for the overall time of
    ;; the entire request...
    (when (or (and (url-request-timeout req)
                   (> (- now (float-time (url-request-start-time req)))
                      (url-request-timeout req)))
              ;; ... and one that's updated whenever new data arrives from the
              ;; server.
              (and (url-request-read-timeout req)
                   (> (- now (float-time (url-request-last-read-time req)))
                      (url-request-read-timeout req))))
      (with-url--callback (url-request-process req)))))

(defun with-url--sentinel (process change)
  (message "%s %s" process change)
  (cond
   ((equal change "open\n")
    (with-url--send-request process))
   ))

(defun with-url--send-request (process)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let* ((req (plist-get (process-plist process) :request))
           (parsed (url-request-parsed-url req)))
      (insert (format "GET %s HTTP/1.1\r\n"
                      (if (zerop (length (url-filename parsed)))
                          "/"
                        (url-filename parsed))))
      (let* ((data (with-url--data req))
             (headers
              (list
               (list "User-Agent" url-user-agent)
               (list "Connection" "close")
               (list "Accept-Encoding"
                     (and (fboundp 'zlib-available-p)
                          (zlib-available-p)
                          "gzip"))
               (list "Accept" "*/*")
               (list "Content-Type" (car data))
               (list "Content-Length" (length (cdr data)))
               (list "Cookies" (and (not (url-request-inhibit-cookies req))
                                    (with-url--cookies parsed)))
               (list "Host" (puny-encode-string (url-host parsed))))))
        (cl-loop for (name value) in headers
                 when (and (not (cl-assoc name (url-request-headers req)
                                          :test #'cl-equalp))
                           value)
                 do (format "%s: %s\n\r" name value))
        (cl-loop for (name value) in (url-request-headers req)
                 when value
                 do (format "%s: %s\n\r" name value))
        (insert "\r\n")
        (when data
          (insert data))))
    (process-send-region process (point-min) (point-max))))

(defun with-url--data (req)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when (url-request-data req)
      (insert (encode-coding-string (url-request-data req)
                                    (url-request-data-charset req)))
      (cl-case (url-request-data-encoding req)
        (url-encode
         (cons "application/x-www-form-urlencoded"
               (mm-url-form-encode-xwfu (buffer-string))))
        (multipart
         (let ((boundary (mml-compute-boundary '())))
           (cons (concat "multipart/form-data; boundary=" boundary)
                 (mm-url-encode-multipart-form-data values boundary))))
        (base64
         (base64-encode-region (point-min) (point-max))
         (cons "application/x-www-form-urlencoded"
               (buffer-string)))))))

(defun with-url--filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (let ((req (plist-get (process-plist process) :request)))
      (setf (url-request-last-read-time req) (current-time))
      ;; Check whether we've got all the data.  We may already have
      ;; saved the response size.
      (unless (url-request-response-size req)
        ;; Get it the hard way.
        (goto-char (point-min))
        (save-match-data
          (let ((case-fold-search t))
            (when-let ((header-end (re-search-forward "^\r?\n" nil t)))
              (goto-char (point-min))
              ;; Only search until header-end since there may be no
              ;; Content-Length header here and we don't want to
              ;; search the contents.
              (cond
               ;; Content-Length header that says what the size is.
               ((re-search-forward "content-length: *\\([0-9]+\\)"
                                   header-end t)
                (let ((size (string-to-number (match-string 1))))
                  (setf (url-request-response-size req)
                        ;; The buffer should end up being the size of
                        ;; the headers plus the body.
                        (+ header-end size -1))))
               ;; No Content-Length; instead the data is passed in
               ;; chunks.
               ((re-search-forward "Transfer-Encoding: *chunked" nil t)
                (goto-char header-end)
                (let (length)
                  (while (looking-at "\\([0-9A-Za-z]+\\)\r?\n")
                    (setq length (string-to-number (match-string 1) 16))
                    (forward-line)
                    (if (zerop length)
                        (setf (url-request-response-size req) (buffer-size))
                      ;; Skip ahead, and then past the CRLF.
                      (goto-char (+ (point) length 2)))))))))))
      (when (and (url-request-response-size req)
                 (>= (buffer-size) (url-request-response-size req)))
        (with-url--process-reply process)))))

(defun url-header (header name)
  (cdr (assq name header)))

(defun with-url--process-reply (process)
  (let* ((headers (with-url--parse-headers))
         (code (car (url-header headers 'http-status)))
         (req (plist-get (process-plist process) :request)))
    (cond
     ;; We got the expected response.
     ((<= 200 code 299)
      (with-url--callback process))
     ;; Redirects.
     ((<= 300 code 399)
      (cl-incf (url-request-redirect-times req))
      (if (> (url-request-redirect-times req) 10)
          (with-url--callback req)
        (with-url--redirect process (url-header headers 'location))))
     )))

(defvar with-url--headers)

(defun with-url--callback (process)
  (message "Calling back")
  (let ((req (plist-get (process-plist process) :request))
        (buffer (process-buffer process)))
    (delete-process process)
    (when (url-request-timer req)
      (cancel-timer (url-request-timer req)))
    (set-process-sentinel process nil)
    (set-process-filter process nil)
    (with-current-buffer buffer
      (let ((headers (with-url--parse-headers)))
        (setq-local with-url--headers headers)
        ;; Delete the headers from the buffer.
        (goto-char (point-min))
        (when (re-search-forward "^\r?\n" nil t)
          (delete-region (point-min) (point)))
        ;; If we have a chunked transfer encoding, then we have to
        ;; remove the chunk length indicators from the response.
        (when (cl-equalp (url-header headers 'transfer-encoding) "chunked")
          (with-url--decode-chunked))
        ;; Text responses should have the CRLF things removed.
        (when (string-match "^text/" (or (url-header headers 'content-type)
                                         "text/html"))
          (goto-char (point-min))
          (while (search-forward "\r\n" nil t)
            (forward-char -1)
            (delete-char -1)))
        (goto-char (point-min))
        (unwind-protect
            (funcall (url-request-callback req))
          (kill-buffer buffer))))))

(defun with-url--decode-chunked ()
  (let (length)
    (goto-char (point-min))
    (while (looking-at "\\([0-9A-Za-z]+\\)\r?\n")
      (setq length (string-to-number (match-string 1) 16))
      (forward-line)
      (delete-region (match-beginning 0) (point))
      (if (zerop length)
          (delete-region (match-beginning 0) (point-max))
        ;; Skip ahead, and then past the CRLF.
        (goto-char (+ (point) length 2))))))

(defun with-url--redirect (process location)
  (let ((req (plist-get (process-plist process) :request)))
    (setf (url-request-url req) location
          (url-request-parsed-url req) nil
          (url-request-response-size req) nil)
    (set-process-sentinel process nil)
    (set-process-filter process nil)
    (when (url-request-timer req)
      (cancel-timer (url-request-timer req)))
    (delete-process process)
    (kill-buffer (process-buffer process))
    (with-url--fetch req)))

(defun with-url--cookies (parsed)
  (mapconcat
   (lambda (cookie)
     (format "%s=%s" (url-cookie-name cookie) (url-cookie-value cookie)))
   ;; Have to sort this for sending most specific cookies first.
   (sort (url-cookie-retrieve (url-host parsed)
                              (url-filename parsed)
                              (equal (url-type parsed) "https"))
         (lambda (x y)
           (> (length (url-cookie-localpart x))
              (length (url-cookie-localpart y)))))
   "; "))

(defun with-url--parse-headers ()
  (goto-char (point-min))
  (let ((headers nil))
    (while (not (looking-at "\r?$"))
      (cond
       ;; The first line is the status line.
       ((null headers)
        ;; Well-formed status line.
        (if (looking-at "\\([^ \n]+\\) +\\([0-9]+\\) +\\([^\r\n]*\\)")
            (push (list 'http-status
                        (string-to-number (match-string 2))
                        (match-string 3)
                        (match-string 1))
                  headers)
          ;; Non-well-formed status line.
          (push (cons 'http-status (buffer-substring
                                    (point)
                                    (and (re-search-forward "\r?$")
                                         (match-beginning 0))))
                headers)))
       ;; Ignore all non-header lines in the header.
       ((looking-at "\\([^\r\n:]+\\): *\\([^\r\n]+\\)")
        (push (cons (intern (downcase (match-string 1)) obarray)
                    (match-string 2))
              headers)))
      (forward-line 1))
    (nreverse headers)))                                                     

(provide 'with-url)

;;; with-url.el ends here
