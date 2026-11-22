(in-package #:doc-extract-backend-docling)

;;; JSON and base64 through the stack: json-protocol (json-backend-jzon) and
;;; encoding-protocol. The exported names stay; no private codec.

(defun base64-encode (octets)
  "RFC 4648 base64 of OCTETS (a string is encoded as UTF-8 first)."
  (encoding-protocol:encode octets :encoding :base64))

(defun base64-decode (string)
  "RFC 4648 base64 STRING → octets."
  (encoding-protocol:decode string :encoding :base64))

(defun decode-json (source)
  "Decode a JSON string or UTF-8 octet vector to hash-tables / vectors.
   Malformed JSON or invalid UTF-8 signals DOC-EXTRACT-ERROR."
  (handler-case
      (etypecase source
        (string (json-protocol:decode source))
        ((vector (unsigned-byte 8))
         (json-protocol:decode
          (encoding-protocol:decode source :encoding :utf-8 :strict t))))
    ((or json-protocol:json-error encoding-protocol:encoding-error) (e)
      (error 'doc-extract-error :message (format nil "docling JSON: ~A" e)))))

(defun encode-json (value)
  "Encode VALUE (alists, hash-tables, vectors) as a JSON string."
  (json-protocol:encode value))
