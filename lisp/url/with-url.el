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
(require 'url-http)

(cl-defstruct url-request
  original-url wait timeout read-timeout
  verbose debug cookies cache ignore-errors
  headers
  method
  data data-charset data-encoding
  callback redirect-times
  url parsed-url process
  response-size start-time last-read-time timer)

(defvar with-url--headers nil)
(defvar with-url--status nil)

(cl-defmacro with-url ((url
                        &key wait timeout
                        read-timeout
                        (verbose 5)
                        (cookies t)
                        (cache t)
                        debug
                        headers
                        ignore-errors
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

Additional keywords can be given to `with-url' to alter its operation.

The returned headers can be examined with the `url-header'
function; the full status with the `url-status' function, and
whether the request returned as expected with the `url-okp' or
`url-errorp' functions.

:wait t
Normal `with-url' operation is asynchronous.  If this parameter
is given, the retrieval will be synchronous instead.  Not all
URLs support asynchronous operation.  In particular, file: and
ftp: documents will always be fetchedh synchronously.

:timeout SECONDS
Give up after approximately SECONDS seconds and execute BODY.

:read-timeout SECONDS
If no data has been received for the last SECONDS seconds, give
up and execute BODY.

:verbose NUMBER
The level of verbosity during operations.  0 will men no messages
are issued.

:debug BOOL
If non-nil, a buffer called \"*url-debug*\" will be created, and
all network traffic, both request and response, is copied to that
buffer.  This buffer may grow very large.

:ignore-errors BOOL
If non-nil, the body will not be executed if the contents
specified by the URL could not be fetched.

:cookies t/read/write/nil
If nil, cookies will neither be sent nor stored.  If `read',
cookies will be recorded, but not sent.  If `write', cookies will
be sent, but not stored.  If nil, no cookie handling will occur.

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

:data-charset CHARSET
What charset (i.e., encoded character set) this data should be
encoded as.  This defaults to `utf-8'.

:data-encoding ENCODING
When using the posting methods, the data is usually encoded in
some fashion.  Supported encodings are `url-form', `multipart'
and `base64'."
  (declare (indent 1))
  (let ((requestv (cl-gensym "request")))
    `(let ((,requestv 
            (make-url-request :original-url ,url
                              :timeout ,timeout
                              :read-timeout ,read-timeout
                              :verbose ,verbose
                              :debug ,debug
                              :cookies ,cookies
                              :cache ,cache
                              :headers ',headers
                              :method ,method
                              :ignore-errors ,ignore-errors
                              :data ,data
                              :data-charset ',data-charset
                              :data-encoding ,data-encoding
                              :start-time (current-time)
                              :last-read-time (current-time)
                              :redirect-times 0)))
       ,(if wait
            `(progn
               (with-url--fetch ,requestv)
               ,@body)
          `(progn
             (setf (url-request-callback ,requestv)
                   (lambda ()
                     ,@body))
             (with-url--fetch ,requestv))))))

(defun url-header (name &optional buffer)
  "Return the value of the specified URL header name from the current buffer.
Example use:

  (url-header 'content-length)

If given, return the value in BUFFER instead."
  (with-current-buffer (or buffer (current-buffer))
    (cdr (assq name with-url--headers))))

(defun url-status (name &optional buffer)
  "Return the status of the URL request in the current buffer.
If given, return the value in BUFFER instead."
  (with-current-buffer (or buffer (current-buffer))
    (cdr (assq name with-url--status))))

(defun url-okp (&optional buffer)
  "Return non-nil if the document was retrieved.
If given, return the value in BUFFER instead."
  (let ((status (url-status 'response buffer)))
    (and status
         (consp status)
         (numberp (car status))
         (<= 200 (car status) 299))))

(defun url-errorp (&optional buffer)
  "Say whether there was an error when retrieving the document.
If given, return the value in BUFFER instead."
  (not (url-okp buffer)))

(defun with-url--fetch (req)
  (unless (url-request-url req)
    (setf (url-request-url req) (url-request-original-url req)))
  (setf (url-request-parsed-url req)
        (url-generic-parse-url (url-request-url req)))
  (pcase (url-type (url-request-parsed-url req))
    ((or "http" "https") (with-url--fetch-http req))
    ("ftp" (with-url--fetch-ftp req))
    ("file" (with-url--fetch-file req))))

(defun with-url--fetch-http (req)
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

(defun with-url--fetch-ftp (req)
  (let ((parsed (url-request-parsed-url req)))
    ;; Transform the URL into Tramp syntax and let it worry about it.
    (with-url--fetch-file
     (concat "/"
             (and (url-user parsed)
                  (format "%s@" (url-user parsed)))
             (url-host)
             (and (url-port parsed)
                  (format "#s" (url-port parsed)))
             ":"
             (url-filename parsed)))))

(defun with-url--fetch-file (req)
  (with-current-buffer (generate-new-buffer "*request*")
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary)
          (buffer (current-buffer)))
      (condition-case err
          (insert-file-contents-literally
           (url-filename (url-request-parsed-url req)))
        (error
         (push (list 'response
                     500 (format "Error occurred while fetching file: %s" err))
               with-url--status)))
      (when (or (not (url-request-ignore-errors req))
                (url-okp))
        (goto-char (point-min))
        (unwind-protect
            (funcall (url-request-callback req))
          (kill-buffer buffer))))))

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
      (with-url--callback (url-request-process req) '(500 "Timer expired")))))

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
               (list "Cookies"
                     (and (memq (url-request-cookies req) '(t write))
                          (with-url--cookies parsed)))
               (list "Host" (puny-encode-string (url-host parsed)))
               (list "If-Modified-Since"
                     (and (memq (url-request-cache req) '(t write))
                          (when-let ((tm (url-is-cached (url-request-url req))))
                            (url-get-normalized-date tm)))))))
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
          (insert data))
        (when (url-request-debug req)
          (with-url--debug 'request (buffer-string)))))
    (process-send-region process (point-min) (point-max))))

(defun with-url--debug (type string)
  (with-current-buffer (get-buffer-create "*url-debug*")
    (insert (if (eq type 'request)
                ">>> "
              "<<< ")
            (format-time-string "%Y%m%dT%H:%M:%S") "\n"
            string)
    (unless (bolp)
      (insert "\n"))
    (insert "----------\n")))

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
               ((re-search-forward "Transfer-Encoding: *chunked" header-end t)
                (goto-char header-end)
                ;; This could be sped up by looking at the end of the
                ;; buffer and see whether there's a 0 length block
                ;; there instead of traversing the entire buffer
                ;; (which may be slow on big documents).
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

(defun with-url--process-reply (process)
  (with-url--parse-headers)
  (let* ((code (car (url-status 'response)))
         (req (plist-get (process-plist process) :request))
         (status (cadr (assq code url-http-codes))))
    ;; Set cookies (if the caller has requested that we record
    ;; cookies, and we've gotten some).
    (when (and (memq (url-request-cookies req) '(t read))
               (url-header 'cookie))
      (url-cookie-handle-set-cookie (url-header 'cookie)))
    (when (url-request-debug req)
      (with-url--debug 'response (buffer-string)))
    (cond
     ;; We got the expected response.
     ((<= 200 code 299)
      (with-url--callback process))
     ;; We don't support proxies.
     ((eq status 'use-proxy)
      (with-url--callback
       req '(500 (format "Redirection through proxy server not supported: %s"
                         (url-header 'location)))))
     ;; The document is in the cache.
     ((eq status 'not-modified)
      (url-cache-extract (url-cache-create-filename (url-request-url req)))
      (with-url--callback process))
     ;; Redirects.
     ((<= 300 code 399)
      (cl-incf (url-request-redirect-times req))
      (if (> (url-request-redirect-times req) 10)
          (with-url--callback req '(500 "Too many redirections"))
        (with-url--redirect process (url-header 'location))))
     (t
      (with-url--callback req)))))

(defun with-url--callback (process &optional status)
  (message "Calling back")
  (let ((req (plist-get (process-plist process) :request))
        (buffer (process-buffer process)))
    (delete-process process)
    (when (url-request-timer req)
      (cancel-timer (url-request-timer req)))
    (set-process-sentinel process nil)
    (set-process-filter process nil)
    (push (cons 'url (url-request-url req)) with-url--status)
    ;; Pass the https certificate on to the caller.
    (when (gnutls-available-p)
      (push (cons 'tls-peer (gnutls-peer-status process)) with-url--status))
    (with-current-buffer buffer
      ;; Allow overriding the status if we have a timeout or the like.
      (when status
        (push (cons 'response status) with-url--status))
      ;; Delete the headers from the buffer.
      (goto-char (point-min))
      (when (re-search-forward "^\r?\n" nil t)
        (delete-region (point-min) (point)))
      ;; If we have a chunked transfer encoding, then we have to
      ;; remove the chunk length indicators from the response.
      (when (cl-equalp (url-header 'transfer-encoding) "chunked")
        (with-url--decode-chunked))
      ;; Text responses should have the CRLF things removed.
      (when (string-match "^text/" (or (url-header 'content-type)
                                       "text/html"))
        (goto-char (point-min))
        (while (search-forward "\r\n" nil t)
          (forward-char -1)
          (delete-char -1)))
      (goto-char (point-min))
      (unwind-protect
          (funcall (url-request-callback req))
        (kill-buffer buffer)))))

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
         (lambda (cookie1 cookie2)
           (> (length (url-cookie-localpart cookie1))
              (length (url-cookie-localpart cookie2)))))
   "; "))

(defun with-url--parse-headers ()
  (goto-char (point-min))
  (setq with-url--status nil
        with-url--headers nil)
  (let ((headers nil))
    (while (not (looking-at "\r?$"))
      (cond
       ;; The first line is the status line.
       ((not with-url--status)
        ;; Well-formed status line.
        (push
         (cons 'response
               (if (looking-at "\\([^ \n]+\\) +\\([0-9]+\\) +\\([^\r\n]*\\)")
                  (list (string-to-number (match-string 2))
                        (match-string 3)
                        (match-string 1))
                ;; Non-well-formed status line.
                (buffer-substring
                 (point)
                 (and (re-search-forward "\r?$")
                      (match-beginning 0)))))
         with-url--status))
       ;; Ignore all non-header lines in the header.
       ((looking-at "\\([^\r\n:]+\\): *\\([^\r\n]+\\)")
        (push (cons (intern (downcase (match-string 1)) obarray)
                    (match-string 2))
              headers)))
      (forward-line 1))
    (setq-local with-url--headers (nreverse headers))
    with-url--headers))

(provide 'with-url)

;;; with-url.el ends here
