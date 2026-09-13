(defpackage #:doc-extract-backend-docling
  (:use #:cl #:doc-extract-protocol)
  (:export #:+convert-source-path+
           #:+docling-priority+
           #:+docling-formats+
           #:*default-docling-endpoint*

           #:docling-backend
           #:make-docling-backend
           #:use-docling-backend
           #:docling-backend-endpoint
           #:docling-backend-timeout
           #:docling-backend-http-fn

           #:docling-http-request
           #:docling-http-request-p
           #:docling-http-request-method
           #:docling-http-request-url
           #:docling-http-request-headers
           #:docling-http-request-body
           #:docling-http-request-timeout

           #:parse-docling-response
           #:docling-document->extracted
           #:document->sections
           #:build-convert-request

           #:encode-json
           #:decode-json
           #:base64-encode
           #:base64-decode))

(in-package #:doc-extract-backend-docling)
